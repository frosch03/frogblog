module Config
    ( pageTitle, styleSheet
    , blogURL, blogPath, blogName
    , pageStep
    , dBase, dBasePort, dBaseViewLocation, dBaseName
    , codeColor
    )
where

import Text.XHtml.Strict 
import Language.Haskell.HsColour.Colourise
import Database.CouchDB (doc, db)
import Network.URI (parseURI, URI)
import Data.Maybe (fromJust, maybe)


pageTitle :: String 
pageTitle = "Confessions of a functional Mind"

styleSheet :: Html
styleSheet = thelink ! [rel "stylesheet", href "/log.css", thetype "text/css"] $ noHtml

blogPath = "http://frosch03.de/"
blogName = "blogfrog.cgi"
blogURL  = blogPath ++ blogName

pageStep :: Int
pageStep = 5 

dBaseServerName     = "frosch03.de"
dBasePort           = 5999 :: Int
dBaseUser           = "frosch03"
dBasePassword       = "SECRET"

dBaseURI            =  "http://"
                    ++ dBaseUser       ++ ":"
                    ++ dBasePassword   ++ "@"
                    ++ dBaseServerName ++ ":" 
                    ++ show dBasePort  ++ "/"

dBase = maybe (error "wrong Database URI") id 
          (parseURI dBaseURI)

dBaseViewLocation   = doc "views"
dBaseName           = db "blogfrog"

codeColor = ColourPrefs
              { keyword          = [Underscore,Foreground Green]
              , keyglyph         = [Foreground Red]
              , layout           = [Foreground Cyan]
              , comment          = [Foreground Blue]
              , conid            = [Foreground White]
              , varid            = [Foreground White]
              , conop            = [Bold,Foreground Red]
              , varop            = [Foreground Cyan]
              , string           = [Foreground Magenta]
              , char             = [Foreground Magenta]
              , number           = [Foreground Magenta]
              , cpp              = [Dim,Foreground Magenta]
              , selection        = [Bold, Foreground Magenta]
              , variantselection = [Dim, Foreground Red, Underscore]
              , definition       = [Foreground White]
              }   
