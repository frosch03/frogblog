module Couch where

import Database.CouchDB
import Database.CouchDB.JSON

import Text.JSON
import Text.JSON.Generic
import Data.Data
import Data.List (nub)

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader 
import Data.Time.Clock.POSIX (getPOSIXTime)

import Blog (BlogEntry)
import Blog.DataDefinition 

import Auxiliary

dBaseServer         = "localhost"
dBasePort           = 5984
dBaseViewLocation   = doc "views"
dBaseName           = db "blog"

type PublishCouch = CouchMonad (Either String Rev)
type FetchCouch a = CouchMonad [(Doc, a)]
type KeyCouch     = CouchMonad [String]
type MetaCouch    = (FetchCouch BlogEntry, ([BlogEntry] -> [String]))



publishPosting :: BlogEntry -> PublishCouch
publishPosting post = 
    do now     <- liftIO getPOSIXTime
       docName <- return $ doc.show $ now
       newNamedDoc dBaseName docName post

fetch :: FetchCouch a -> IO [a]
fetch query =
    do dBaseOutput <- runCouchDB dBaseServer dBasePort query
       return (map snd dBaseOutput)

fetchMeta :: MetaCouch -> IO [String]
fetchMeta (query, f) =
    do dBaseOutput  <- fetch query
       dBaseOutput' <- return $ f dBaseOutput
       return (dBaseOutput')

limitTo :: IO [BlogEntry] -> MetaData -> IO [BlogEntry]
limitTo bs (From author) = 
    do bs' <- bs
       return $ filter f bs'
    where f :: BlogEntry -> Bool
          f (Entry mds _) = (peel $ getMeta isAuthor mds) == author
limitTo bs (To cats) = 
    do bs' <- bs
       return $ filter f bs'
    where f :: BlogEntry -> Bool
          f (Entry mds _) = foldl1 (||) $ map (flip elem $ (peel_ $ getMeta isCategory mds)) cats
limitTo bs (Subject sub) = 
    do bs' <- bs
       return $ filter f bs'
    where f :: BlogEntry -> Bool
          f (Entry mds _) = (peel $ getMeta isSub mds) == sub



type View = String
type Reverse = Bool
query :: (JSON a) => View -> Reverse -> FetchCouch a
query view r = 
    do dBaseView <- return (doc view)
       queryView dBaseName dBaseViewLocation dBaseView [("descending", JSBool r)]

byDateTimeR :: FetchCouch BlogEntry
byDateTimeR = query "allPosts" True

bySubject :: FetchCouch BlogEntry
bySubject = query "bySubject" False

byCategory :: FetchCouch BlogEntry
byCategory = query "byCategory" False

allCategories :: MetaCouch
allCategories = (byDateTimeR, onlyCategories)

allAuthors :: MetaCouch
allAuthors = (byDateTimeR, onlyAuthors)



onlyCategories :: [BlogEntry] -> [String]
onlyCategories = nub . concat . map (peelCatName . excerpCategorie)
    where excerpCategorie (Entry md _) = getMeta isCategory md
          peelCatName     (To xs)      = xs

onlyAuthors :: [BlogEntry] -> [String]
onlyAuthors = nub . map (peelAuthorName . excerpAutor)
    where excerpAutor (Entry md _) = getMeta isAuthor md
          peelAuthorName (From x)  = x
