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
import Blog.Text (shorten)
import Couch ( fetch, limitTo
             , byDateTimeR, bySubject)
import Page -- (simplePosts, posts)

main :: IO ()
main = do d <- current 
          runCGI $ handleErrors (cgiMain d)

cgiMain :: Date -> CGI CGIResult
cgiMain d
    = do filter <- getFilter
         date   <- return d
         renderPageByState (BS date filter)

--

getFilter :: CGI Filter
getFilter
    = try "page"      (return . ThisPage . read)
    $ try "subject"   (return . ThisSubject)
    $ try "author"    (return . ThisAuthor)
    $ try "category"  (return . ThisCategory)
    $ (return LatestByDate)
    

current :: IO Date
current 
    = do now  <- getCurrentTime
         day  <- return (formatTime defaultTimeLocale "%d" now)
         mon  <- return (formatTime defaultTimeLocale "%m" now)
         year <- return (formatTime defaultTimeLocale "%Y" now)
         return $ D ((read year), (read mon), (read day))


getAllEntrys  v     = lift $ fetch v
getSomeEntrys v p f = lift $ fetch v `p` f

genAbstract e = return $ map (shorten 5) e

renderPagedPosting state v page
    = do entrys    <- getAllEntrys v
         abstracts <- genAbstract entrys
         output $ renderHtml (posts state page abstracts) 

renderSingelPost state v p f 
    = do entrys  <- getSomeEntrys v p f
         output $ renderHtml (simplePosts state entrys)

renderSimpleAbstracts state v p f
    = do entrys    <- getSomeEntrys v p f 
         abstracts <- genAbstract entrys
         output $ renderHtml (simplePosts state abstracts)



renderPageByState :: BlogState -> CGI CGIResult
renderPageByState state@(BS date (ThisPage cnt))
    = renderPagedPosting state byDateTimeR cnt

renderPageByState state@(BS date (ThisSubject sub))
    = renderSingelPost state bySubject limitTo (Subject sub)

renderPageByState state@(BS date (ThisAuthor author))
    = renderSimpleAbstracts state byDateTimeR limitTo (From author)

renderPageByState state@(BS date (ThisCategory cat))
    = renderSimpleAbstracts state byDateTimeR limitTo (To [cat])

renderPageByState state@(BS date LatestByDate)
    = renderPagedPosting state byDateTimeR 0

try :: String -> (String -> CGI Filter) -> CGI Filter -> CGI Filter
try s f def = do tmp <- getInput s
                 maybe def f tmp 
