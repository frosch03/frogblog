module Main where

import Network.CGI
import Text.XHtml (renderHtml)
import Control.Monad.Trans (lift)
 
import Blog
import Blog.DataDefinition
import Couch
import Page

main :: IO ()
main = runCGI $ handleErrors (cgiMain)



cgiMain :: CGI CGIResult
cgiMain = 
    do category <- getInput "category"
       category <- return (Just "test")
       maybe pageDefault postsByCategory category 



pageDefault :: CGI CGIResult
pageDefault =
    do entrys <- lift $ queryEntrys
       output $ renderHtml (pages entrys)

postsByCategory :: Category -> CGI CGIResult
postsByCategory cat =
    do metas <- lift $ queryOneCategory cat 
       output $ renderHtml (headings metas)
