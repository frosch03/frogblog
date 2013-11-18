module Snippets.Twitter 
    ( snip ) 
where

import Text.XHtml.Strict

snip :: Html
snip = pT

pT :: Html
pT =     anchor ! [ theclass "twitter-timeline"
                  , href     "https://twitter.com/frosch03"
                  , strAttr  "data-widget-id" "402027882446217216"
                  , width    "200"
                  , height   "300"
                  , strAttr  "data-chrome" "nofooter transparent"
                  , strAttr  "data-tweet-limit" "5"
                  , strAttr  "data-link-color" "#808080"
                  ] 
         $ stringToHtml "Tweets by @frosch03"
     +++ primHtml twitterScript
    where
      twitterScript = "<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+\"://platform.twitter.com/widgets.js\";fjs.parentNode.insertBefore(js,fjs);}}(document,\"script\",\"twitter-wjs\");</script>"
