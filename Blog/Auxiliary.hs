module Blog.Auxiliary 
    ( linkify
    , dropOK
    -- , textLink
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

linkify :: (Parameter, Value, Html) -> Html
linkify ([],  val, htmltext) = toHtml $ hotlink blogURL htmltext
linkify (par, val, htmltext) = toHtml $ hotlink (blogURL ++ ('?': par) ++ ('=' : val)) htmltext

-- textLink :: (Parameter, Value, String) -> BlogText
-- textLink ([],  _,   _   ) = Empty
-- textLink (par, val, text) = PureC 
--                              (Link 
--                                (blogURL ++ ('?': par) ++ ('=': val))
--                                (PureT text)
--                              )   

dropOK :: Result a -> a
dropOK (Ok x)    = x 
dropOK (Error x) = error x
