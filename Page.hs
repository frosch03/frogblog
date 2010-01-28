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
renderPostings (b:bs) =   paragraph << b
                      +++ renderPostings bs

page :: BlogEntry -> Html
page b = pages [b]

pages :: [BlogEntry] -> Html
pages = site renderPostings


renderHeadings :: [[MetaData]] -> Html
renderHeadings []       = noHtml
renderHeadings (md:mds) =   ( paragraph 
                            <<  (   sub
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
