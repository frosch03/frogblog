module Snippets.StaticLinks 
    ( snip ) 
where

import Text.XHtml.Strict

snip :: Html
snip = asciiTable

heading :: String
heading = "Static Content"

strings :: [(String, String)]
strings = [ ("old frosch03.de", "http://frosch03.de/blogfrog.cgi?subject=Alter%20Kontent")
          , ("Pad: Mahlzeit",   "http://frosch03.de/pad/Mahlzeit.html")
          , ("Groupchat Links", "http://frosch03.de/linklist/groupchat.html")
          ]

minLength :: Int
minLength = 23

maxLength :: Int
maxLength = foldl1 max $ minLength : (map length $ heading : (map fst strings))

asciiTable :: Html
asciiTable
    =     s hr    +++ br
      +++ s (between sl sr (fillUp maxLength heading)) +++ br
      +++ s hr    +++ br
      +++ s empty +++ br
      +++ (map staticLinks strings)
      +++ s empty +++ br
      +++ s hr    +++ br
    where staticLinks = (\(n, u) -> s (sl ++ " * ") 
                               +++ (anchor ! [href u] $ s n ) 
                               +++ s (replicate (maxLength - (length n) - 3) ' ') 
                               +++ s sr 
                               +++ br
                        )
          between = (\l r s -> l ++ s ++ r)
          fillUp = (\i s -> s ++ (replicate (i - (length s)) ' '))
          empty = between sl sr $ replicate maxLength ' '
          hr = between el er $ replicate maxLength '-'
          el = "+-"
          er = "-+"
          sl = "| "
          sr = " |"
          s  = stringToHtml
