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
    = try "page"     (page d)
    $ try "subject"  (postWithSubject d)
    $ try "author"   (postsByAuthor  d)
    $ try "category" (postsByCategory d)
    $ pageDefault d

--
data Filter 
    = LatestByDate
    | ThisMonth
    | LastMonth
    | ThisYearByMonth
    | BySubject

current :: IO Date
current 
    = do now  <- getCurrentTime
         day  <- return (formatTime defaultTimeLocale "%d" now)
         mon  <- return (formatTime defaultTimeLocale "%m" now)
         year <- return (formatTime defaultTimeLocale "%Y" now)
         return $ D ((read year), (read mon), (read day))
--

try :: String -> (String -> CGI CGIResult) -> CGI CGIResult -> CGI CGIResult
try s f def = do tmp <- getInput s
                 maybe def f tmp 

page :: Date -> String -> CGI CGIResult
page d p = 
    do entrys  <- lift $ fetch byDateTimeR
       entrys' <- return $ map (shorten 5) entrys
       p'      <- return $ ((read p) :: Int)
       output $ renderHtml (posts d p' entrys') 

pageDefault :: Date -> CGI CGIResult
pageDefault d =
    do entrys  <- lift $ fetch byDateTimeR
       entrys' <- return $ map (shorten 5) entrys
       output $ renderHtml (posts d 0 entrys')
--       output $ renderHtml (simplePosts entrys')

postWithSubject :: Date -> Category -> CGI CGIResult
postWithSubject d sub =
    do entry <- lift $ fetch bySubject `limitTo` (Subject sub)
       output $ renderHtml (simplePosts d entry)

postsByCategory :: Date -> Category -> CGI CGIResult
postsByCategory d cat =
    do entrys  <- lift $ fetch byDateTimeR `limitTo` (To [cat])
       entrys' <- return $ map (shorten 5) entrys
       output $ renderHtml (simplePosts d entrys')

postsByAuthor :: Date -> Author -> CGI CGIResult
postsByAuthor d author =
    do entrys  <- lift $ fetch byDateTimeR `limitTo` (From author)
       entrys' <- return $ map (shorten 5) entrys
       output $ renderHtml (simplePosts d entrys')
