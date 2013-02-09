module Couch 
    ( publishPosting
    , deletePosting
    , fetch
    , fetchMeta
    , limitTo
    , byDateTimeR
    , bySubject
    , allCategories
    , allAuthors
    , allSubjects
    , PublishCouch
    , runCouch

    , onlySubjects
    , getAllEntrys
    , getSomeEntrys
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

import Text.Regex.Posix

-- Intern
import Blog (BlogEntry)
import Blog.Definition
import Auxiliary (peel, peel_, getMeta, isSub, isAuthor, isCategory, isDate)
import Config (dBase, dBaseName, dBaseViewLocation)
import Filter
import BlogState


type PublishCouch = CouchMonad (Either String Rev)
type FetchCouch a = CouchMonad [(Doc, a)]
type DeleteCouch  = CouchMonad Bool
type KeyCouch     = CouchMonad [String]
type CouchQuery   = (FetchCouch BlogEntry, ([BlogEntry] -> [String]))

class Limitable a where
    limitTo :: IO [BlogEntry] -> a -> IO [BlogEntry]

instance Limitable MetaData where
    limitTo = limitToMeta

instance Limitable Filter where
    limitTo = limitToFilter

    
runCouch = runCouchDBURI

fetch :: FetchCouch a -> IO [a]
fetch query =
    do dBaseOutput <- runCouch dBase query
       return (map snd dBaseOutput)

getAllEntrys  v     = lift $ fetch v
getSomeEntrys v p f = lift $ fetch v `p` f

limitToFilter :: IO [BlogEntry] -> Filter -> IO [BlogEntry]
limitToFilter bs (ThisMonth month) =
    do bs' <- bs
       return $ filter f bs' 
    where f :: BlogEntry -> Bool
          f (Entry mds _) = d =~ ("[0-9][0-9]-"++month++"-.*")
            where d = (peel $ getMeta isDate mds) 

limitToMeta :: IO [BlogEntry] -> MetaData -> IO [BlogEntry]
limitToMeta bs (From author) =
    do bs' <- bs
       return $ filter f bs'
    where f :: BlogEntry -> Bool
          f (Entry mds _) = (peel $ getMeta isAuthor mds) == author
limitToMeta bs (To cats) =
    do bs' <- bs
       return $ filter f bs'
    where f :: BlogEntry -> Bool
          f (Entry mds _) = foldl1 (||) $ map (flip elem $ (peel_ $ getMeta isCategory mds)) cats
limitToMeta bs (Subject sub) =
    do bs' <- bs
       return $ filter f bs'
    where f :: BlogEntry -> Bool
          f (Entry mds _) = (peel $ getMeta isSub mds) == sub

byDateTimeR :: FetchCouch BlogEntry
-- byDateTimeR = query "allPosts" True
byDateTimeR = query "allByDate" True

bySubject :: FetchCouch BlogEntry
bySubject = query "bySubject" False

allCategories :: CouchQuery
allCategories = (byDateTimeR, onlyCategories)

allAuthors :: CouchQuery
allAuthors = (byDateTimeR, onlyAuthors)

allSubjects :: CouchQuery
allSubjects = (bySubject, onlySubjects)




publishPosting :: BlogEntry -> PublishCouch
publishPosting post =
    do now     <- liftIO getPOSIXTime
       docName <- return $ doc.show $ now
       newNamedDoc dBaseName docName post

deletePosting :: MetaData -> IO Bool 
deletePosting (Subject sub) = 
    do dBaseOutput  <- runCouch dBase bySubject
       (matchID, _) <- return . head $ filter f dBaseOutput
       runCouch dBase $ forceDeleteDoc dBaseName matchID
    where f (_, (Entry mds _)) = (peel $ getMeta isSub mds) == sub


fetchMeta :: CouchQuery -> IO [String]
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

onlySubjects :: [BlogEntry] -> [String]
onlySubjects = nub . map (peelSubName . excerpSubject)
    where excerpSubject (Entry md _) = getMeta isSub md
          peelSubName   (Subject xs) = xs
