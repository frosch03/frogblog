module Page where

import Text.XHtml.Strict hiding (sub)

import Blog
import FrogBlog
import HtmlSnippets
import Auxiliary

htmlHead :: Html
htmlHead =  header 
         << (   thetitle << (stringToHtml pageTitle)
            +++ styleSheet
            )

site :: (a -> Html) -> a -> Html
site f x =   htmlHead
         +++ body
         <<  (   primHtml pageHead
             +++ primHtml pageNav
             +++ ( thediv ! [theclass "box"]
                   $ thediv ! [theclass "blogblock"]
                     $ f x
                 )
             +++ primHtml pageFoot 
             )

renderPostings :: [BlogEntry] -> Html
renderPostings []     = noHtml
renderPostings (b:bs) =   b
                      +++ renderPostings bs

page :: BlogEntry -> Html
page b = pages [b]

pages :: [BlogEntry] -> Html
pages = site renderPostings

----

type Page = (Int, Int)
display :: [BlogEntry] -> Int -> Html
display bs actPage = site_ (actPage, maxPage) renderPostings displayed_bs
    where displayed_bs = (take pageStep) . (drop skip) $ bs
          skip         = pageStep * actPage 
          maxPage      = (length bs) `div` pageStep

site_ :: Page -> (a -> Html) -> a -> Html
site_ (actPage, maxPage) f x =   htmlHead
                             +++ body
                             <<  (   primHtml pageHead
                                 +++ primHtml pageNav
                                 +++ ( thediv ! [theclass "box"]
                                       $ thediv ! [theclass "blogblock"]
                                         $ (   ( thediv ! [theclass "prevnext"] $
                                                   (if isNext then next else stringToHtml "[ older ")
                                               +++ (if isPrev then prev else stringToHtml "| newer ]")
                                               )
                                           +++ f x
                                           +++ ( thediv ! [theclass "prevnext"] $
                                                   (if isNext then next else stringToHtml "[ older ")
                                               +++ (if isPrev then prev else stringToHtml "| newer ]")
                                               )
                                           +++ br
                                           )
                                     )
                                 +++ primHtml pageFoot 
                                 )
    where isNext  = (actPage < maxPage)
          isPrev  = (actPage > 0)
          next    = toHtml $ hotlink (blogURL ++ ('?': "page") ++ ('=': show (actPage+1))) (stringToHtml "[ older ")
          prev    = toHtml $ hotlink (blogURL ++ ('?': "page") ++ ('=': show (actPage-1))) (stringToHtml "| newer ]")

----

renderHeadings :: [[MetaData]] -> Html
renderHeadings []       = noHtml
renderHeadings (md:mds) =   (   (   sub
                                +++ date 
                                +++ stringToHtml " by " 
                                +++ from
                                )
                            )
                        +++ renderHeadings mds
    where sub  = getMeta isSub  md
          date = getMeta isDate md
          from = getMeta isFrom md

heading :: [MetaData] -> Html
heading m = headings [m]

headings :: [[MetaData]] -> Html
headings = site renderHeadings
