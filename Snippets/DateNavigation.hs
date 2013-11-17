module Snippets.DateNavigation
    ( fsnip ) 
where

import Config
import BlogState
import Text.XHtml.Strict

fsnip :: Date -> Html
fsnip = dynNav


dynNav :: Date -> Html
dynNav (D (year, month, day))
    =   s "  Blog Navigation"   +++ br
    +++ s "-------------------" +++ br
    +++ (foldl1 (+++) lnk)
    where s = stringToHtml
          lnk = map (\(u, n) -> (s "  * ") +++ (anchor ! [href u] $ (s n)) +++ br) $
                [ (blogPath ++ ("/page/0"),                                     "latest")
                , (blogPath ++ ("/month/") ++ (month2string month),             "this month")
                , (blogPath ++ ("/month/") ++ (month2string $ lastMonth month), "last month")
                ]
          month2string m = if m < 10 then '0': (show m)
                                     else (show m)
          lastMonth m = if m == 1 then 1 else (m-1)
