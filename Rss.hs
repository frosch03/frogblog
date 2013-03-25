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

mzuri = (URI "http:" (Just $ URIAuth "" "frosch03.de" "") "/category/mahlzeit" "" "")

mzEmptyRss
  = RSS
    "frosch03.de/category/mahlzeit"
    froguri
    "Mahlzeit"
    fbChanE
    []

mzChanE
  = [ Language "de"
    , WebMaster "frosch03@gmail.com"
    , Generator "frogblog"
    , TextInput "frosch03.de/cat/mahlzeit" "Mahlzeit" "" froguri
    ] 

-- mzRss :: [BlogEntry] -> String
-- mzRss es = header ++ ((showXML.rssToXML) $ froggerss fbEmptyRss es)
--   where header
--           = concat $ 
--               "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
--             : "<rss xmlns:itunes=\"http://www.itunes.com/dtds/podcast-1.0.dtd\" version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
--             : [[]]
--         itunesHeader




-- mzRssHead :: RSS -> [BlogEntry] -> RSS
-- <title>Mahlzeit
-- <link>http://frosch03.de/category/Mahlzeit
-- <language>de
-- <copyright>Sebastian Raedle und Matthias Brettschneider 
-- <atom:link href="http://frosch03.de/blogfrog.cgi?rssmahlzeit" ...
-- <itunes:subtitle>Gespräche einer Mittagspause
-- <itunes:author>Matthias Brettschneider, Sebastian Rädle
-- <itunes:summary>Mahlzeit sind die Gespräche aus der Mittagspause von hackel und frosch, rund um Themen aus der Informatik und was uns sonst noch einfällt
-- <itunes:explicit>no
-- <itunes:description>Mahlzeit nehmen wir in der unserer Mittagspause auf. Es gibt auch immer einen Videostream, der aber nicht notwenigerweise benötigt wird. Wir sprechen über Themen rund um die Informatik und ziehen gerne paralellen in die afkWelt. 
-- <itunes:owner>
-- <itunes:name>Matthias und Sebastian
-- <itunes:email>mahlzeit@frosch03.de
-- </itunes:owner>

-- <itunes:image href="http://frosch03.de/img/..." />
-- <itunes:category text="Technic &amp; Science" />

-- <item>

-- <title>MZ01: Kompositionen, bash und Pizza
-- <itunes:author>Matthias Brettschneider, Sebastian Rädle
-- <itunes:subtitle>Zu Funktionskompositionen in bash und Haskell 
-- <itunes:summary>Zu Funktionskompositionen in bash und Haskell 

-- <description>Zu Funktionskompositionen in bash und Haskell 
-- <enclosure url="http://frosch03.de/Mahlzeit/MZ01.mp3" length? type="audio/mp3"
-- <link>http://frosch03.de/id/....
-- <guid>http://frosch03.de/Mahlzeit/MZ01.mp3
-- <pubDate>Weekday, Day Month Year Hour:Min:Sec GMT (Thu, 1 Nov 2012 12:00:00 GMT
-- <itunes:duration>...
-- <itunes:keywords>
-- <itunes:explicit>no


-- </item>
