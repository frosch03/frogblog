module Blog.HTML 
    ()
where

-- Extern
import Text.XHtml.Strict
import Language.Haskell.HsColour.InlineCSS
import Text.Pandoc as P
import Text.Blaze.Html.Renderer.String as BStr
import Data.Set (insert)

-- Intern
import Blog.Definition
import Blog.Auxiliary (linkify) --, textLink)
import Auxiliary (peel, getMeta, isSub, isDate, isFrom, isTo)
import Config
import HtmlSnippets (livefyreSnip)

instance HTML (BlogEntry) where
    toHtml (Entry meta entry) = entryToHtml (toHtml $ getMeta isSub meta)  meta entry


entryToHtml :: Html -> [MetaData] -> Pandoc -> Html
entryToHtml heading meta entry
    = thediv ! [theclass "entry"] 
    $     (if (isLong entry) then (heading) else (linkify ("blog", subj, heading)))
      +++ toHtml date +++ br
      +++ pandoc_html +++ br
      +++ hr
      +++ (stringToHtml "From: " +++ toHtml from +++ br) 
      +++ (stringToHtml "To: "   +++ toHtml to   +++ br)
      +++ if (isLong entry) then (hr +++ comments) else (br)
    where date = getMeta isDate meta
          from = getMeta isFrom meta
          to   = getMeta isTo   meta
          (Subject subj) = getMeta isSub  meta
          pandoc_html    = primHtml $ BStr.renderHtml (P.writeHtml def' entry)
          comments       = primHtml $ livefyreSnip
          isLong         = (\(Pandoc _ x) -> (> 5) $ length x)
          def'           = P.def { writerExtensions = insert Ext_tex_math_dollars (writerExtensions P.def) }          
          




instance HTML (MetaData) where
    toHtml (Subject x)  = h1 << (toHtml x)
    toHtml (Date    x)  = thespan ! [theclass "date"]     $ toHtml x
    toHtml (From    x)  = thespan ! [theclass "author"]   $ linkify ("author", x, stringToHtml x)
    toHtml (To  (x:[])) = thespan ! [theclass "category"] $ linkify ("category", x, stringToHtml x)
    toHtml (To  (x:xs)) = thespan ! [theclass "category"] $ foldl fun (linkify ("category", x, stringToHtml x)) xs
        where fun :: Html -> String -> Html
              fun done []   = done
              fun done next = done +++ (stringToHtml ", " +++ linkify ("category", next, stringToHtml next))

