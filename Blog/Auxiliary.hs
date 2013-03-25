module Blog.Auxiliary 
    ( linkify
    , dropOK
    , toId
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
linkify ([],  val, htmltext) = toHtml $ hotlink blogPath htmltext
linkify (par, val, htmltext) = toHtml $ hotlink (blogPath ++ ('/': par) ++ ('/' : val)) htmltext

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


toId :: String -> String
toId d = year ++ month ++ day ++ hour ++ minute
  where year   = take 4 d
        month  = take 2 . drop  5 $ d
        day    = take 2 . drop  8 $ d
        hour   = take 2 . drop 11 $ d
        minute = take 2 . drop 14 $ d

