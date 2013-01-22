module Auxiliary 
    ( peel, peel_
    , getMeta
    , isSub, isDate, isTo,       isFrom 
                   , isCategory, isAuthor
    , fromOK
    , firstUp

    -- , genAbstract
    -- , shorten
    )
where

-- Extern 
import Data.Char (toLower, toUpper)
import Text.JSON (Result(..))

-- Intern
import Blog.Definition
import Blog.Text
import Blog.Auxiliary

--genAbstract e = return $ map (shorten 5) e

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


-- shorten :: Int -> BlogEntry -> BlogEntry
-- shorten n (Entry md post) = Short md (fromLines appendix short)
--     where lns      = lines $ toText post
--           short    = take n lns 
--           sub      = peel $ getMeta isSub md
--           appendix = (textLink ("subject", sub, " (more)... "))
