module FrogBlog where

import Text.XHtml.Strict

pageTitle :: String 
pageTitle = "Confessions of a functional Mind"

styleSheet :: Html
styleSheet = thelink ! [rel "stylesheet", href "/log.css", thetype "text/css"] $ noHtml
