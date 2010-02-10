module Config
    ( pageTitle, styleSheet
    , blogURL, blogPath, blogName
    , pageStep
    , dBaseServer, dBasePort, dBaseViewLocation, dBaseName
    , codeColor
    )
where

-- Extern
import Text.XHtml.Strict 
import Language.Haskell.HsColour.Colourise
import Database.CouchDB (doc, db)

pageTitle :: String 
pageTitle = "Confessions of a functional Mind"

styleSheet :: Html
styleSheet = thelink ! [rel "stylesheet", href "/log.css", thetype "text/css"] $ noHtml

blogPath = "http://localhost/cgi-bin/"
blogName = "testBlog.cgi"
blogURL  = blogPath ++ blogName

pageStep :: Int
pageStep = 5 

dBaseServer         = "localhost"
dBasePort           = 5984 :: Int
dBaseViewLocation   = doc "views"
dBaseName           = db "blog"


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
