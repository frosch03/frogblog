module Couch 
    ( fetch
    , limitTo
    , byDateTimeR
    , bySubject
    , byCategory
    , allCategories
    , allAuthors
    )
where

-- Extern 
import Database.CouchDB
import Database.CouchDB.JSON

import Text.JSON
import Text.JSON.Generic

import Data.Data
import Data.List

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader

import Data.Time.Clock.POSIX (getPOSIXTime)

-- Intern
import Blog (BlogEntry)
import Blog.Definition
import Auxiliary (peel, peel_, getMeta, isSub, isAuthor, isCategory)
import Config (dBaseServer, dBasePort, dBaseName, dBaseViewLocation)


type PublishCouch = CouchMonad (Either String Rev)
type FetchCouch a = CouchMonad [(Doc, a)]
type KeyCouch     = CouchMonad [String]
type MetaCouch    = (FetchCouch BlogEntry, ([BlogEntry] -> [String]))

fetch :: FetchCouch a -> IO [a]
fetch query =
    do dBaseOutput <- runCouchDB dBaseServer dBasePort query
       return (map snd dBaseOutput)

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




publishPosting :: BlogEntry -> PublishCouch
publishPosting post =
    do now     <- liftIO getPOSIXTime
       docName <- return $ doc.show $ now
       newNamedDoc dBaseName docName post

fetchMeta :: MetaCouch -> IO [String]
fetchMeta (query, f) =
    do dBaseOutput  <- fetch query
       dBaseOutput' <- return $ f dBaseOutput
       return (dBaseOutput')

type View = String
type Reverse = Bool
query :: (JSON a) => View -> Reverse -> FetchCouch a
query view r =
    do dBaseView <- return (doc view)
       queryView dBaseName dBaseViewLocation dBaseView [("descending", JSBool r)]



onlyCategories :: [BlogEntry] -> [String]
onlyCategories = nub . concat . map (peelCatName . excerpCategorie)
    where excerpCategorie (Entry md _) = getMeta isCategory md
          peelCatName     (To xs)      = xs

onlyAuthors :: [BlogEntry] -> [String]
onlyAuthors = nub . map (peelAuthorName . excerpAutor)
    where excerpAutor (Entry md _) = getMeta isAuthor md
          peelAuthorName (From x)  = x

