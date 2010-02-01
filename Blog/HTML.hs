module Blog.HTML where

import Text.XHtml.Strict
import Language.Haskell.HsColour.InlineCSS

import Blog.DataDefinition
import Auxiliary
import FrogBlog


instance HTML (BlogEntry) where
    toHtml (Entry meta entry) = thediv ! [theclass "entry"] 
                              $     (toHtml sub +++ toHtml date +++ br)
                                +++ (toHtml entry +++ br)
                                +++ hr
                                +++ (stringToHtml "From: " +++ toHtml from +++ br)
                                +++ (stringToHtml "To: " +++ toHtml to +++ br)
        where sub  = getMeta isSub  meta
              date = getMeta isDate meta
              from = getMeta isFrom meta
              to   = getMeta isTo   meta

instance HTML (MetaData) where
    toHtml (Subject x)  = h1 << (toHtml x)
    toHtml (Date    x)  = thespan ! [theclass "date"]     $ toHtml x
    toHtml (From    x)  = thespan ! [theclass "author"]   $ linkify ("author", x)
    toHtml (To  (x:[])) = thespan ! [theclass "category"] $ linkify ("category", x)
    toHtml (To  (x:xs)) = thespan ! [theclass "category"] $ foldl fun (linkify ("category", x)) xs
        where fun :: Html -> String -> Html
              fun done []   = done
              fun done next = done +++ (stringToHtml ", " +++ linkify ("category", next))

instance HTML (BlogText) where
    toHtml (Empty)          = noHtml
    toHtml (PureT txt)      = stringToHtml txt 
    toHtml (PureC cmd)      = toHtml cmd
    toHtml (MixT  txt rest) = (stringToHtml txt) +++ (toHtml rest)
    toHtml (MixC  cmd rest) = (toHtml       cmd) +++ (toHtml rest)

instance HTML (Command) where
    toHtml (None)               = noHtml
    toHtml (Break)              = br
    toHtml (Bold      body)     = bold       << (toHtml body)
    toHtml (Italic    body)     = italics    << (toHtml body)
    toHtml (Underline body)     = thespan  ! [theclass "uline"]  $ (toHtml body)
    toHtml (Strike    body)     = thespan  ! [theclass "strike"] $ (toHtml body)
    toHtml (Section   body)     = h2         << (toHtml body)
    toHtml (Link      url body) = toHtml   $ hotlink url (toHtml body)
    toHtml (Code      src)      = thediv   ! [theclass "code"]
                                $ primHtml $ hscolour codeColor False src


type Parameter = String
type Value     = String
linkify :: (Parameter, Value) -> Html
linkify ([],  val) = toHtml $ hotlink blogURL (stringToHtml val)
linkify (par, val) = toHtml $ hotlink (blogURL ++ ('?' : par) ++ ('=' : val)) (stringToHtml val)
