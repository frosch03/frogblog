module Couch where

import Database.CouchDB
import Database.CouchDB.JSON

import Text.JSON
import Text.JSON.Generic
import Data.Data

import Control.Monad.Trans (liftIO)
--import Control.Monad.Reader (ReaderT, ask, liftM3)
import Control.Monad.Reader 
import Data.Time.Clock.POSIX (getPOSIXTime)

import Blog (BlogEntry)
import Blog.DataDefinition 

import Auxiliary


-- TODO:
-- funktion to write a blogpost into the database
-- function to read the actual and n previous blogpost's from the database
-- function to read a specific and n previous blogpost's from the database
-- ^^- the given functions but also with sorting by subject, category, author 
-- ^^- the given functions but also with filtering by subject, category, author, date
-- function to overwrite a specific blogpost in the database
-- function to delete a specific blogpost in the database

type BlogCouchMonad = ReaderT DB CouchMonad (Either String Rev)

publishTest :: CouchMonad (Either String Rev)
publishTest = 
    do s       <- liftIO (readFile "/tmp/test.hlog")
       posting <- return $ read s :: CouchMonad BlogEntry
       (postEntry posting) `runReaderT` (db "blog") 

postEntry :: BlogEntry -> BlogCouchMonad
postEntry posting = 
    do now     <- liftIO getPOSIXTime
       docName <- return $ doc.show $ now
       dBase   <- ask
       lift $ newNamedDoc dBase docName posting
       

queryEntrys :: IO [BlogEntry]
queryEntrys = 
    do x      <- runCouchDB' $ queryView (db "blog") (doc "views") (doc "bySubject") [("descending", JSBool True)]
       return (map snd x)

queryLastEntry :: IO BlogEntry
queryLastEntry = 
    do x <- queryEntrys
       return (head x)

queryMetaData :: IO [[MetaData]]
queryMetaData = 
    do x      <- runCouchDB' $ queryView (db "blog") (doc "views") (doc "onlyMeta") [("descending", JSBool True)]
       return (map snd x)

queryOneCategory :: Category -> IO [[MetaData]]
queryOneCategory cat = 
      do result  <- queryMetaData
         return $ excerpt cat result



excerpt :: Category -> [[MetaData]] -> [[MetaData]]
excerpt cat = foldr (\a b -> if (hasCategory cat a) then a:b else b) []

hasCategory :: Category -> [MetaData] -> Bool
hasCategory cat md = foldl1 (||) $ map (== cat) metaTo
    where metaTo     :: [Category]
          (To metaTo) = getMeta isTo md
