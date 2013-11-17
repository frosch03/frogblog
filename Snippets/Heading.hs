module Snippets.Heading
    ( snip )
where

import Text.XHtml.Strict

snip :: Html
snip = pageHead

pageHead :: Html
pageHead 
    = thediv ! [identifier "top"] $
      ( thelink ! [href "http://frosch03.de/blog"] $
        ( image ! [ alt "frosch03.de/blog"
                  , src "http://frosch03.de/img/blog.gif"
                  ]
        )
      ) 
