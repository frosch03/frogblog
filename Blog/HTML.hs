module Blog.HTML where

import Text.XHtml.Strict

import Blog.DataDefinition
import Auxiliary


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
    toHtml x@(Subject _)  = h1 << (toHtml x)
    toHtml x@(Date    _)  = thespan ! [theclass "date"]     $ toHtml x
    toHtml x@(From    _)  = thespan ! [theclass "author"]   $ toHtml x
    toHtml (To      xs) = thespan ! [theclass "category"] $ stringToHtml $ commaSeperate xs
        where commaSeperate xs = (drop 2) . (foldr (\next done -> (", " ++ next) ++ done) "") $ xs

instance HTML (BlogText) where
    toHtml (Empty)          = noHtml
    toHtml (PureT txt)      = stringToHtml txt 
    toHtml (PureC cmd)      = toHtml cmd
    toHtml (MixT  txt rest) = (stringToHtml txt) +++ (toHtml rest)
    toHtml (MixC  cmd rest) = (toHtml       cmd) +++ (toHtml rest)

instance HTML (Command) where
    toHtml (None)               = noHtml
    toHtml (Break)              = br
    toHtml (Bold      body)     = bold      << (toHtml body)
    toHtml (Italic    body)     = italics   << (toHtml body)
    toHtml (Underline body)     = thespan ! [theclass "uline"]  $ (toHtml body)
    toHtml (Strike    body)     = thespan ! [theclass "strike"] $ (toHtml body)
    toHtml (Link      url body) = toHtml $ hotlink url (toHtml body)
