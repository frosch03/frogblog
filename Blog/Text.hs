module Blog.Text 
    ( shorten
    , fromLines)
where

-- Intern
import Blog.Definition
import Blog.Auxiliary (textLink)
import Auxiliary (peel, getMeta, isSub)
import Config

class TEXT a where
    toText :: a -> String

instance TEXT (BlogEntry) where
    toText (Entry md post) = toText post

instance TEXT (BlogText) where
    toText (Empty)          = ""
    toText (PureT txt)      = txt 
    toText (PureC cmd)      = toText cmd 
    toText (MixT  txt rest) = txt          ++ (toText rest)
    toText (MixC  cmd rest) = (toText cmd) ++ (toText rest)

instance TEXT (Command) where
    toText (None)               = ""  
    toText (Break)              = ['\n']
    toText (Block     body)     = ("\n\n") ++ (toText body)
    toText (Bold      body)     = (toText body)
    toText (Italic    body)     = (toText body)
    toText (Underline body)     = (toText body)
    toText (Strike    body)     = (toText body)
    toText (Section   body)     = (toText body)
    toText (Link      url body) = '[':(toText body) ++ "]" 
    toText (Code      src)      = src 
    toText (Itemize   bodys)    = foldl1 (\r n -> r ++ "  * " ++ n) $ map toText bodys
--    toText (Itemize   bodys)    = map ( ((flip (++)) "\n") . ((++) "  *") . toText ) bodys




shorten :: Int -> BlogEntry -> BlogEntry
shorten n (Entry md post) = Entry md (fromLines appendix short)
    where lns      = lines $ toText post
          short    = take n lns 
          sub      = peel $ getMeta isSub md
          appendix = (textLink ("subject", sub, " (more)... "))

type Appendix = BlogText
fromLines :: Appendix -> [String] -> BlogText
fromLines apx (ln: []) = MixT ln apx 
fromLines apx (ln:lns) = MixT ln (MixC Break (fromLines apx lns))
