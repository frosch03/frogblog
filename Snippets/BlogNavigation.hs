module Snippets.BlogNavigation
    ( snip ) 
where

import Text.XHtml.Strict

snip :: Html
snip = pageNav

pageNav :: Html
pageNav
    =   s "  Table of Content"    +++ br
    +++ s "---------------------" +++ br
    +++ blgLnk
    +++ (foldl1 (+++) $ zipWith (\x y -> s ("  " ++ (show x) ++ ") ") +++ y) [2..] lnk)
    where s   = stringToHtml
          blgLnk
              =     s "  1) " +++ (anchor ! [href "http://frosch03.de"]                  $ s "Blog") 
                +++ s " ("    +++ (anchor ! [href "http://m.frosch03.de"]                $ s "mobile") +++ s ")"
                +++ s " ("    +++ (anchor ! [href "http://frosch03.de/blogfrog.cgi?rss"] $ s "rss")    +++ s ")" +++ br
          lnk = map (\(u, n) -> anchor ! [href u] $ (s n) +++ br) $
                [ ("http://frosch03.de/pad",  "Pads")
                , ("http://pics.frosch03.de", "Pictures")
                ]
