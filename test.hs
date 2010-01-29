module Main where

import Network.CGI
import Network.CGI.Protocol
import Text.XHtml (renderHtml)
import Control.Monad (join)
import Control.Monad.Trans (lift)
import Data.Maybe (isJust, fromJust)
 
import Blog
import Blog.DataDefinition
import Couch
import Page

main :: IO ()
main = runCGI $ handleErrors (cgiMain)

cgiMain :: CGI CGIResult
cgiMain = try "author"   postsByAuthor 
        $ try "category" postsByCategory
        $ pageDefault

try :: String -> (String -> CGI CGIResult) -> CGI CGIResult -> CGI CGIResult
try s f def = do tmp <- getInput s
                 maybe def f tmp

pageDefault :: CGI CGIResult
pageDefault =
    do entrys <- lift $ fetch byDateTimeR
       output $ renderHtml (pages entrys)

postsByCategory :: Category -> CGI CGIResult
postsByCategory cat =
    do entrys <- lift $ fetch byDateTimeR `limitTo` (To [cat])
       output $ renderHtml (pages entrys)

postsByAuthor :: Author -> CGI CGIResult
postsByAuthor author =
    do entrys <- lift $ fetch byDateTimeR `limitTo` (From author)
       output $ renderHtml (pages entrys)
