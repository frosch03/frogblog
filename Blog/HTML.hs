module Blog.HTML where

import Blog.DataDefinition
import Auxiliary

type HTML = String

class Wrap a where
    enHTML :: a -> HTML

instance Wrap (Command) where
    enHTML (None)           = ""  
    enHTML (Break)          = "<br />"
    enHTML (Bold      body) = "<b>"      ++ enHTML body ++ "</b>"
    enHTML (Italic    body) = "<i>"      ++ enHTML body ++ "</i>"
    enHTML (Underline body) = "<u>"      ++ enHTML body ++ "</u>"
    enHTML (Strike    body) = "<strike>" ++ enHTML body ++ "</strike>"
    enHTML (Link      url  body) = "<a href=\"" ++ url ++ "\">" ++ enHTML body ++ "</a>"

instance Wrap (BlogText) where
    enHTML (Empty)          = ""
    enHTML (PureT txt)      = txt 
    enHTML (PureC cmd)      = enHTML cmd
    enHTML (MixT  txt rest) = txt ++ enHTML rest
    enHTML (MixC  cmd rest) = enHTML cmd ++ enHTML rest

instance Wrap (MetaData) where
    enHTML (Subject x) = "<h1>"                      ++ x ++ "</h1>"
    enHTML (Date x)    = "<span class=\"date\">"     ++ x ++ "</span>"
    enHTML (To xs)     = "<span class=\"category\">" ++ ((drop 2).(foldr (\n o ->  (", " ++ n) ++ o) "") $ xs)
                      ++ "</span>"
    enHTML (From x)    = "<span class=\"author\">"   ++ x ++ "</span>"

instance Wrap (BlogEntry) where
    enHTML (Entry meta entry) =  codeblockB
                              ++ enHTML sub ++ enHTML date ++ nl
                              ++ enHTML entry
                              ++ nl ++ "<hr>"
                              ++ "From: " ++ enHTML from ++ nl
                              ++ "To: " ++ enHTML to ++ nl
                              ++ codeblockE
        where sub  = getMeta isSub  meta
              date = getMeta isDate meta
              from = getMeta isFrom meta
              to   = getMeta isTo   meta

divB = "<div>"
divE = "</div>"

nl = "<br />"

layerB = "<div class=\"layer\">"
layerE = divE

codeblockB = "<div class=\"codeblock\">"
codeblockE = divE

subjectB = "<h3>"
subjectE = "</h3>"
