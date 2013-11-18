module Snippets.Flattr 
    ( snip )
where

import Text.XHtml.Strict

snip :: Html
snip = flattr

flattrURL :: String
flattrURL = "http://flattr.com/thing/1147770/frosch03-de-confessions-of-a-functional-mind"

flattr :: Html
flattr = anchor ! [href flattrURL, thestyle "background-color: #333333;"] $ 
         image  ! [alt "Flattr this", title "Flattr this", src "http://api.flattr.com/button/flattr-badge-large.png"]
