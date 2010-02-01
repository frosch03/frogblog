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
import Page hiding (page)
import Text

main :: IO ()
main = runCGI $ handleErrors (cgiMain)

cgiMain :: CGI CGIResult
cgiMain = try "page"     page
        $ try "subject"  postWithSubject
        $ try "author"   postsByAuthor 
        $ try "category" postsByCategory
        $ pageDefault

try :: String -> (String -> CGI CGIResult) -> CGI CGIResult -> CGI CGIResult
try s f def = do tmp <- getInput s
                 maybe def f tmp

page :: String -> CGI CGIResult
page p =
    do entrys  <- lift $ fetch byDateTimeR
       entrys' <- return $ map (shorten 5) entrys
       p'      <- return $ ((read p) :: Int)
       output $ renderHtml (display entrys' p')

pageDefault :: CGI CGIResult
pageDefault =
    do entrys  <- lift $ fetch byDateTimeR
       entrys' <- return $ map (shorten 5) entrys
       output $ renderHtml (display entrys' 0)
--       output $ renderHtml (pages entrys')

postWithSubject :: Category -> CGI CGIResult
postWithSubject sub =
    do entry <- lift $ fetch bySubject `limitTo` (Subject sub)
       output $ renderHtml (pages entry)

postsByCategory :: Category -> CGI CGIResult
postsByCategory cat =
    do entrys <- lift $ fetch byDateTimeR `limitTo` (To [cat])
       output $ renderHtml (pages entrys)

postsByAuthor :: Author -> CGI CGIResult
postsByAuthor author =
    do entrys <- lift $ fetch byDateTimeR `limitTo` (From author)
       output $ renderHtml (pages entrys)
