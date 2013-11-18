module Page
where

-- Extern 
import Text.XHtml.Strict hiding (sub)
import Network.CGI (output)

-- Snippets
import Snippets.Heading        as Heading
import Snippets.BlogNavigation as BlogNav
import Snippets.DateNavigation as DateNav
import Snippets.StaticLinks    as Static
import Snippets.Flattr         as Flattr
import Snippets.Twitter        as Twitter

-- Intern
import Blog
import Auxiliary (getMeta, isFrom, isSub, isTo, isDate, genAbstract)
import HtmlSnippets
import Config
import Couch (getAllEntrys, getSomeEntrys)
import Filter
import BlogState
import Rss

type Counts = (Int, Int)

type Navigation = Html


renderRss state v
    = do entrys <- getAllEntrys v
         output $ genRss entrys

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



mobilePagedPosting state v page
    = do entrys    <- getAllEntrys v
         abstracts <- genAbstract entrys
         output $ renderHtml (postsM state page abstracts)

mobileSingelPost state v p f 
    = do entrys  <- getSomeEntrys v p f
         output $ renderHtml (mobilePosts state entrys)

mobileSimpleAbstracts state v p f
    = do entrys    <- getSomeEntrys v p f 
         abstracts <- genAbstract entrys
         output $ renderHtml (mobilePosts state abstracts)


simplePosts :: BlogState -> [BlogEntry] -> Html
simplePosts (BS date _) = simpleSite date renderPostings


mobilePosts :: BlogState -> [BlogEntry] -> Html
mobilePosts (BS date _) = mobileSite date renderPostings


posts :: BlogState -> Int -> [BlogEntry] -> Html
posts (BS date _) pageNum bs = site date counts renderPostings bs'
    where bs'     = (take pageStep) . (drop skip) $ bs
          skip    = pageStep * pageNum
          pageMax = (length bs) `div` pageStep
          counts  = (pageNum, pageMax)

site :: Date -> Counts -> (a -> Html) -> a -> Html
site date cs f = simpleSite date $ pnNavi cs f



postsM :: BlogState -> Int -> [BlogEntry] -> Html
postsM (BS date _) pageNum bs = siteM date counts renderPostings bs'
    where bs'     = (take pageStep) . (drop skip) $ bs
          skip    = pageStep * pageNum
          pageMax = (length bs) `div` pageStep
          counts  = (pageNum, pageMax)

siteM :: Date -> Counts -> (a -> Html) -> a -> Html
siteM date cs f = mobileSite date $ pnNavi cs f


mobileSite :: Date -> (a -> Html) -> a -> Html
mobileSite date f x
    =   htmlHead
    +++ body
    <<
    ( ( thediv ! [theclass "center"] $
        ( thediv ! [theclass "blogblock"] $
          f x
        )
      )
      +++
      ( thediv ! [identifier "footer"] $
        br
      )
    )



simpleSite :: Date -> (a -> Html) -> a -> Html
simpleSite date f x 
    =   htmlHead
    +++ body
    <<
    ( ( thediv ! [identifier "wrapper"] $
        ( thediv ! [identifier "header"] $
          Heading.snip
        )
        +++
        ( thediv ! [identifier "middle"] $
          ( thediv ! [identifier "container"] $
            ( thediv ! [identifier "content"] $
              ( thediv ! [theclass "center"] $ 
                ( thediv ! [theclass "blogblock"] $ 
                  f x
                )
              )
            )
          )
          +++
          ( thediv ! [identifier "sideLeft", theclass "sidebar"] $
            navigation date
          )
          +++
          ( thediv ! [identifier "sideRight", theclass "sidebar"] $
            ( thediv ! [identifier "rightContainer"] $ 
              ( ( pre ! [theclass "statics"] $ 
                      Static.snip 
                  +++ Flattr.snip
                )
                +++ Twitter.snip
              )
            )
          )
        ) 
      )
      +++
      ( thediv ! [identifier "footer"] $
        br
      ) 
    )


renderPostings :: [BlogEntry] -> Html
renderPostings []     = noHtml
renderPostings (b:bs) =   b
                      +++ renderPostings bs


htmlHead :: Html
htmlHead =  header
         << (   thetitle << (stringToHtml pageTitle)
            +++ styleSheet
            +++ (primHtml latexMathML)
	    +++ (primHtml playerScript)
            +++ (primHtml flattrSnip)
            )


navigation :: Date -> Html
navigation d
    = thediv ! [identifier "leftContainer"] $
        thediv ! [identifier "left"] $
        (   pre ! [theclass "navi"] $ 
            BlogNav.snip
            +++ br
            +++ (fsnip d)
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
          next    = toHtml $ hotlink (blogPath ++ ("/page/") ++ (show (actPage+1))) (stringToHtml "[ older ")
          prev    = toHtml $ hotlink (blogPath ++ ("/page/") ++ (show (actPage-1))) (stringToHtml "| newer ]")
