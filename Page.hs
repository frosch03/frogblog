module Page
    ( simplePosts 
    , posts
    )
where

-- Extern 
import Text.XHtml.Strict hiding (sub)

-- Intern
import Blog
import Auxiliary (getMeta, isFrom, isSub, isTo, isDate)
import HtmlSnippets
import Config

type Counts = (Int, Int)


simplePosts :: [BlogEntry] -> Html
simplePosts = simpleSite renderPostings

posts :: Int -> [BlogEntry] -> Html
posts pageNum bs = site counts renderPostings bs'
    where bs'     = (take pageStep) . (drop skip) $ bs
          skip    = pageStep * pageNum
          pageMax = (length bs) `div` pageStep
          counts  = (pageNum, pageMax)


navigation :: Html
navigation 
    = thediv ! [theclass "left"] $
    (   pre ! [theclass "navi"] $ primHtml pageNav
    +++ br
    +++ primHtml pageNav
    )

simpleSite :: (a -> Html) -> a -> Html
simpleSite f x =   htmlHead
         +++ body
         <<  (   primHtml pageHead
             +++ navigation
             +++ (   thediv ! [theclass "box"]
                     $ thediv ! [theclass "blogblock"]
                       $ f x
                 )
             -- +++ primHtml iFrameInstagram
             +++ primHtml pageFoot
             )

site :: Counts -> (a -> Html) -> a -> Html
site cs f = simpleSite $ pnNavi cs f


renderPostings :: [BlogEntry] -> Html
renderPostings []     = noHtml
renderPostings (b:bs) =   b
                      +++ renderPostings bs


htmlHead :: Html
htmlHead =  header
         << (   thetitle << (stringToHtml pageTitle)
            +++ styleSheet
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


-- needed ???
renderHeadings :: [[MetaData]] -> Html
renderHeadings []       = noHtml
renderHeadings (md:mds) =   (   (   sub
                                +++ date
                                +++ stringToHtml " by "
                                +++ from
                                )
                            )
                        +++ renderHeadings mds
    where sub  = getMeta isSub  md
          date = getMeta isDate md
          from = getMeta isFrom md

headings :: [[MetaData]] -> Html
headings = simpleSite renderHeadings
-------------

