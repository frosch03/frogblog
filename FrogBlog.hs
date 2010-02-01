module FrogBlog where

import Text.XHtml.Strict
import Language.Haskell.HsColour.Colourise

pageTitle :: String 
pageTitle = "Confessions of a functional Mind"

styleSheet :: Html
styleSheet = thelink ! [rel "stylesheet", href "/log.css", thetype "text/css"] $ noHtml

blogPath = "http://localhost/cgi-bin/"
blogName = "testBlog.cgi"
blogURL  = blogPath ++ blogName

pageStep :: Int
pageStep = 5

codeColor = ColourPrefs
              { keyword  = [Underscore,Foreground Green]
              , keyglyph = [Foreground Red]
              , layout   = [Foreground Cyan]
              , comment  = [Foreground Blue]
              , conid    = [Normal]
              , varid    = [Normal]
              , conop    = [Bold,Foreground Red]
              , varop    = [Foreground Cyan]
              , string   = [Foreground Magenta]
              , char     = [Foreground Magenta]
              , number   = [Foreground Magenta]
              , cpp      = [Dim,Foreground Magenta]
              , selection = [Bold, Foreground Magenta]
              , variantselection = [Dim, Foreground Red, Underscore]
              , definition = [Normal]
              }
