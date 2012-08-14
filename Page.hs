module Page
where

-- Extern 
import Text.XHtml.Strict hiding (sub)
import Network.CGI (output)

-- Intern
import Blog
import Auxiliary (getMeta, isFrom, isSub, isTo, isDate, genAbstract)
import HtmlSnippets
import Config
import Couch (getAllEntrys, getSomeEntrys)
import Filter
import BlogState

type Counts = (Int, Int)

type Navigation = Html


renderPagedPosting state v page
    = do entrys    <- getAllEntrys v
         abstracts <- genAbstract entrys
         output $ renderHtml (posts state page abstracts) 

renderSingelPost state v p f 
    = do entrys  <- getSomeEntrys v p f
         output $ renderHtml (simplePosts state entrys)

renderSimpleAbstracts state v p f
    = do entrys    <- getSomeEntrys v p f 
         abstracts <- genAbstract entrys
         output $ renderHtml (simplePosts state abstracts)


simplePosts :: BlogState -> [BlogEntry] -> Html
simplePosts (BS date _) = simpleSite date renderPostings

posts :: BlogState -> Int -> [BlogEntry] -> Html
posts (BS date _) pageNum bs = site date counts renderPostings bs'
    where bs'     = (take pageStep) . (drop skip) $ bs
          skip    = pageStep * pageNum
          pageMax = (length bs) `div` pageStep
          counts  = (pageNum, pageMax)


site :: Date -> Counts -> (a -> Html) -> a -> Html
site date cs f = simpleSite date $ pnNavi cs f


simpleSite :: Date -> (a -> Html) -> a -> Html
simpleSite date f x =   htmlHead
         +++ body
         <<  (   primHtml pageHead
             +++ navigation date
             +++ (   thediv ! [theclass "box"]
                     $ thediv ! [theclass "blogblock"]
                       $ f x
                 )
             -- +++ primHtml iFrameInstagram
             +++ primHtml pageFoot
             )

renderPostings :: [BlogEntry] -> Html
renderPostings []     = noHtml
renderPostings (b:bs) =   b
                      +++ renderPostings bs


htmlHead :: Html
htmlHead =  header
         << (   thetitle << (stringToHtml pageTitle)
            +++ styleSheet
            )



dynNav :: Date -> Html
dynNav (D (year, month, day))
    =          stringToHtml "  Blog Navigation"
    +++ br +++ stringToHtml "-------------------"
    +++ br +++ stringToHtml "  * " +++ toHtml (hotlink (blogURL ++ ('?': "page=0")) (stringToHtml "latest"))
    +++ br +++ stringToHtml "  * " +++ toHtml (hotlink (blogURL ++ ('?': "filter") ++ ('=': (show month))) (stringToHtml "this month"))
    +++ br +++ stringToHtml "  * " +++ stringToHtml "last month"
    +++ br +++ stringToHtml "  * " +++ stringToHtml "this year by month"
    +++ br +++ stringToHtml "  * " +++ stringToHtml "all articles (by subject)"


staticNav :: Html
staticNav = primHtml pageNav

navigation :: Date -> Html
navigation d
    = thediv ! [theclass "left"] $
    (   pre ! [theclass "navi"] $ 
            staticNav
        +++ br
        +++ dynNav d
    )

pnNavi :: Counts -> (a -> Html) -> a -> Html
pnNavi cs f x = pnWrap cs +++ f x +++ pnWrap cs

pnWrap :: Counts -> Html
pnWrap (actPage, maxPage) = 
    thediv ! [theclass "prevnext"] $
         (   (if isNext then next else stringToHtml "[ oldr ")
         +++ (if isPrev then prev else stringToHtml "| newr ]")
         )
    where isNext  = (actPage < maxPage)
          isPrev  = (actPage > 0)
          next    = toHtml $ hotlink (blogURL ++ ('?': "page") ++ ('=': show (actPage+1))) (stringToHtml "[ older ")
          prev    = toHtml $ hotlink (blogURL ++ ('?': "page") ++ ('=': show (actPage-1))) (stringToHtml "| newer ]")
