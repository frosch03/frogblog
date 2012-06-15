module Blog.Auxiliary 
    ( linkify
    , dropOK
    , textLink
    )
where

-- Extern
import Text.JSON ( Result(..) )
import Text.XHtml.Strict

-- Inter
import Blog.Definition
import Config

type Parameter = String
type Value     = String

linkify :: (Parameter, Value, String) -> Html
linkify ([],  val, text) = toHtml $ hotlink blogURL (stringToHtml text)
linkify (par, val, text) = toHtml $ hotlink (blogURL ++ ('?' : par) ++ ('=' : val)) (stringToHtml text)

textLink :: (Parameter, Value, String) -> BlogText
textLink ([],  _,   _   ) = Empty
textLink (par, val, text) = PureC 
                             (Link 
                               (blogURL ++ ('?': par) ++ ('=': val))
                               (PureT text)
                             )   

dropOK :: Result a -> a
dropOK (Ok x)    = x 
dropOK (Error x) = error x