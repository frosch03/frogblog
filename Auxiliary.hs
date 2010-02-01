module Auxiliary where

import Data.Char (toLower, toUpper)

import Blog.DataDefinition
import Text.JSON (Result(..))

peel :: MetaData -> String
peel (Subject s) = s
peel (Date    s) = s
peel (From    s) = s
peel (To      s) = concat s

peel_ :: MetaData -> [String]
peel_ (To      s) = s

getMeta :: (MetaData -> Bool) -> [MetaData] -> MetaData
getMeta f = head.(filter f)

isSub :: MetaData -> Bool
isSub (Subject _) = True
isSub _           = False

isDate :: MetaData -> Bool
isDate (Date _) = True
isDate _        = False

isTo :: MetaData -> Bool
isTo (To _) = True
isTo _      = False

isFrom :: MetaData -> Bool
isFrom (From _) = True
isFrom _        = False

fromOK :: Result a -> a
fromOK (Ok x)    = x
fromOK (Error x) = error $ "fromOK wasn't OK: " ++ x

isAuthor    = isFrom
isCategory  = isTo

firstUp :: [String] -> [String]
firstUp []     = []
firstUp (x:xs) = (x_first : (tail x_low)) : firstUp xs
    where x_low   = (map toLower x)
          x_first = toUpper $ head x_low

