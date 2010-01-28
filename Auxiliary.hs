module Auxiliary where

import Blog.DataDefinition
import Text.JSON (Result(..))

peel :: MetaData -> String
peel (Subject s) = s
peel (Date    s) = s
peel (From    s) = s
peel (To      s) = concat s

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
