module Page where

import Text.XHtml hiding (text, sub)
import qualified Text.XHtml as XHTML (text, sub)

import Blog
import FrogBlog
import HtmlSnippets
import Auxiliary

htmlHead :: Html
htmlHead =  header 
         << (   thetitle << (stringToHtml pageTitle)
            +++ (primHtml styleSheet)
            )

site :: (a -> Html) -> a -> Html
site f x =   htmlHead
         +++ body
         <<  (   primHtml pageHead
             +++ primHtml pageNav
             +++ primHtml "<div class=\"box\">"
             +++ primHtml "<div class=\"layer\">"
             +++ f x
             +++ primHtml "</div>"
             +++ primHtml "</div>"
             +++ primHtml pageFoot 
             )


renderPostings :: [BlogEntry] -> Html
renderPostings []     = primHtml ""
renderPostings (b:bs) = (paragraph.primHtml $ enHTML b)
                     +++ renderPostings bs

page :: BlogEntry -> Html
page b = pages [b]

pages :: [BlogEntry] -> Html
pages = site renderPostings


renderHeadings :: [[MetaData]] -> Html
renderHeadings []       = primHtml ""
renderHeadings (md:mds) =   ( paragraph 
                            <<  (   ( h1 $ primHtml sub)
                                +++ ( primHtml $ date ++ " by " ++ from)
                                )
                            )
                        +++ renderHeadings mds
    where sub  = peel $ getMeta isSub  md
          date = peel $ getMeta isDate md
          from = peel $ getMeta isFrom md

heading :: [MetaData] -> Html
heading m = headings [m]

headings :: [[MetaData]] -> Html
headings = site renderHeadings
