module Couch where

import Database.CouchDB
import Database.CouchDB.JSON

import Text.JSON
import Text.JSON.Generic
import Data.Data

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader 
import Data.Time.Clock.POSIX (getPOSIXTime)

import Blog (BlogEntry)
import Blog.DataDefinition 

import Auxiliary

-- publishPosting       :: BlogEntry -> BloggerCouch
-- fetchBlog byDateTime :: IO [BlogEntry]
-- fetchBlog bySubject  :: IO [BlogEntry]
-- fetchMeta authors    :: IO [MetaData]
-- fetchMeta categorys  :: IO [MetaData]

dBaseServer         = "localhost"
dBasePort           = 5984
dBaseViewLocation   = doc "views"
dBaseName           = db "blog"

type PublishCouch = CouchMonad (Either String Rev)
type FetchCouch a = CouchMonad [(Doc, a)]

publishPosting :: BlogEntry -> PublishCouch
publishPosting post = 
    do now     <- liftIO getPOSIXTime
       docName <- return $ doc.show $ now
       newNamedDoc dBaseName docName post

fetchBlog :: FetchCouch BlogEntry -> IO [BlogEntry]
fetchBlog = fetch

fetchMeta :: FetchCouch MetaData -> IO [MetaData]
fetchMeta = fetch

fetch :: FetchCouch a -> IO [a]
fetch query =
    do dBaseOutput <- runCouchDB dBaseServer dBasePort query
       return (map snd dBaseOutput)

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
