module Rss where

-- Extern
import Text.RSS
import Network.URI
import Data.Time

-- Intern
import Config
import Auxiliary
import Blog.Text
import Blog.Definition (BlogEntry(..))

  
froguri = (URI "http:" (Just $ URIAuth "" "frosch03.de" "") "/blog" "" "")

fbEmptyRss
  = RSS
    "frosch03.de/blog"
    froguri
    pageTitle
    fbChanE
    []

fbChanE
  = [ Language "de"
    , WebMaster "frosch03@gmail.com"
    , Generator "frogblog"
    , TextInput "frosch03.de/blog" pageTitle "" froguri
    ] 

genRss :: [BlogEntry] -> String
genRss es = header ++ ((showXML.rssToXML) $ froggerss fbEmptyRss es)
  where header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

froggerss :: RSS -> [BlogEntry] -> RSS
froggerss (RSS t u p c _) entrys
  = let is = foldl f [] entrys
     in (RSS t u p c is)
  where sub  md    = peel $ getMeta isSub  md
        from md    = peel $ getMeta isFrom md
        date md    = peel $ getMeta isDate md
        link sub   = (URI "http:" (Just $ URIAuth "" "frosch03.de" "") ("/blog/" ++ sub) "" "")
        desc entry = toText $ shorten 5 entry
        f is (Entry mds pd)   = ( [ Title (sub mds)
                                  , Link  (link.sub $ mds)
                                  , Description (desc (Entry mds pd))
                                  , Author ((from mds) ++ "@gmail.com")
                                  , PubDate (convDayTime $ date mds)
                                  ]
                                : is
                                )
convDayTime time
  = let utctimestring = (\xs -> (take 10 xs) ++ (' ':(take 2 $ drop 11 xs)) ++ (':':(take 2 $ drop 14 xs)) ++ ":00.000000 UTC" ) time
    in  read utctimestring :: UTCTime
