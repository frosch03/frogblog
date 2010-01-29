module Text where 

import Blog
import Blog.DataDefinition

shorten :: Int -> BlogEntry -> BlogEntry
shorten n (Entry md post) = Entry md (lines2post short)
    where lns   = lines $ toText post
          short = take n lns

lines2post :: [String] -> BlogText
lines2post []       = Empty
lines2post (ln:lns) = MixT ln (MixC Break (lines2post lns))

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
    toText (Bold      body)     = (toText body)
    toText (Italic    body)     = (toText body)
    toText (Underline body)     = (toText body)
    toText (Strike    body)     = (toText body)
    toText (Link      url body) = '[':(toText body) ++ "]"
