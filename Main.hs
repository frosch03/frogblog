module Main where

-- Extern 
import Network.CGI
import Network.CGI.Protocol

import Text.XHtml (renderHtml)

import Control.Monad (join)
import Control.Monad.Trans (lift)

import Data.Maybe (isJust, fromJust)

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

-- Intern
import Blog
import Blog.Definition
import Page
import Rss
import Filter
import BlogState
import Config hiding (blogURL, blogPath, blogName)
import Couch ( fetch, limitTo
             , byDateTimeR, byDateTime, bySubject
             , byId)


main :: IO ()
main = do d <- current 
          runCGI $ handleErrors (cgiMain d)

cgiMain :: Date -> CGI CGIResult
cgiMain d
    = do filter <- getFilter
         date   <- return d
         renderPageByState (BS date filter)

getFilter :: CGI Filter
getFilter
    = try "page"      (return . ThisPage . read)
    $ try "subject"   (return . ThisSubject)
    $ try "id"        (return . ThisId)
    $ try "author"    (return . ThisAuthor)
    $ try "category"  (return . ThisCategory)
    $ try "month"     (return . ThisMonth)
    $ try "rss"       (return . (const GenRss))
    $ (return LatestByDate)
    

current :: IO Date
current 
    = do now  <- getCurrentTime
         day  <- return (formatTime defaultTimeLocale "%d" now)
         mon  <- return (formatTime defaultTimeLocale "%m" now)
         year <- return (formatTime defaultTimeLocale "%Y" now)
         return $ D ((read year), (read mon), (read day))






renderPageByState :: BlogState -> CGI CGIResult

renderPageByState state@(BS date GenRss)
    = renderRss state byDateTime

renderPageByState state@(BS date (ThisPage cnt))
    = renderPagedPosting state byDateTimeR cnt

renderPageByState state@(BS date (ThisSubject sub))
    = renderSingelPost state bySubject limitTo (Subject sub)

renderPageByState state@(BS date (ThisId id))
    = renderSingelPost state byId limitTo (ThisId id)

renderPageByState state@(BS date (ThisAuthor author))
    = renderSimpleAbstracts state byDateTimeR limitTo (From author)

renderPageByState state@(BS date (ThisCategory cat))
    = renderSimpleAbstracts state byDateTimeR limitTo (To [cat])

renderPageByState state@(BS date LatestByDate)
    = renderPagedPosting state byDateTimeR 0

renderPageByState state@(BS date (ThisMonth month))
    = renderSimpleAbstracts state byDateTimeR limitTo (ThisMonth month)

try :: String -> (String -> CGI Filter) -> CGI Filter -> CGI Filter
try s f def = do tmp <- getInput s
                 maybe def f tmp 
