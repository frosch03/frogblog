module Main 
    ()
where

import System.Environment
import System.Directory
import System.IO
import System.FilePath.Posix (isValid)
import Data.List (lookup, find)

import Database.CouchDB (runCouchDB)

import Blog
import Blog.Definition
import Couch (PublishCouch, publishPosting, deletePosting, fetchMeta, allSubjects)
import Config

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("publish", publish)
            , ("list",    list)
            , ("delete",  delete)
            ]

runCouchDB' :: PublishCouch -> IO ()
runCouchDB' couch = 
    do runCouchDB dBaseServer dBasePort couch
       return ()

main :: IO ()
main =
    do (arguments)       <- getArgs
       (cmd:args)        <- if (length arguments > 0) then return arguments else error $ "to few arguments"
       case (lookup cmd dispatch) of
           (Just action)  -> action args
           (Nothing)      -> error $ "unknown command " ++ cmd

publish :: [String] -> IO ()
publish (filename:_) = 
    if (not . isValid $ filename) 
        then error $ filename ++ " is not a valid FilePath"
        else do content    <- readFile filename
                blogEntry  <- return $ (read content :: BlogEntry)
                runCouchDB' $ publishPosting blogEntry

       

list :: [String] -> IO ()
list _ = 
    do xs <- fetchMeta allSubjects
       sequence_ $ map putStrLn xs
       

delete :: [String] -> IO ()
delete (sub:_) = 
    do deletePosting (Subject sub)
       return ()
       
