module BlogText where

import Text.ParserCombinators.Parsec

import Text.JSON
import Database.CouchDB.JSON ( jsonField
                             , jsonObject
                             , jsonString
                             )


type URL  = String
type Text = String

data BlogEntry 
    = Entry [MetaData] BlogText
    deriving (Show)

data MetaData 
    = Subject  String
    | Date     String
    | To [String]
    | From   String
    deriving (Show)


data BlogText 
    = PureT Text
    | PureC Command
    | MixT  Text BlogText
    | MixC  Command BlogText
    | Empty 
    deriving (Show)


data Command
    = Bold      BlogText
    | Italic    BlogText
    | Underline BlogText
    | Strike    BlogText
    | Link URL  BlogText
    | None
    deriving (Show)
           
instance Read (BlogEntry) where
    readsPrec p s = case parse pBlogEntry "" s of
                        Left  _ -> []
                        Right x -> [(x, "")]

instance Read (BlogText) where
    readsPrec p s = case parse pBlogText "" s of
                        Left  _ -> []
                        Right x -> [(x,"")]

instance Read (Command) where
    readsPrec p s = case parse pCommand "" s of 
                        Left  _ -> []
                        Right x -> [(x, "")]

type HTML = String

class Wrap a where
    enHTML :: a -> HTML

instance Wrap (Command) where
    enHTML (None)           = "" 
    enHTML (Bold      body) = "<b>"      ++ enHTML body ++ "</b>"
    enHTML (Italic    body) = "<i>"      ++ enHTML body ++ "</i>"
    enHTML (Underline body) = "<u>"      ++ enHTML body ++ "</u>"
    enHTML (Strike    body) = "<strike>" ++ enHTML body ++ "</strike>"
    enHTML (Link      url  body) = "<a href=\"" ++ url ++ "\">" ++ enHTML body ++ "</a>"

instance Wrap (BlogText) where
    enHTML (Empty)          = ""
    enHTML (PureT txt)      = txt
    enHTML (PureC cmd)      = enHTML cmd
    enHTML (MixT  txt rest) = txt ++ enHTML rest
    enHTML (MixC  cmd rest) = enHTML cmd ++ enHTML rest


peel js key = (fromOK.(jsonField key)) $ fromOK.jsonObject $ js
    where fromOK (Ok x)    = x
          fromOK (Error x) = error x

instance JSON (BlogEntry) where
    showJSON x = JSObject $ toJSObject [ ("MetaData", JSArray $ map showJSON meta)
                                       , ("Posting",  showJSON post)
                                       ]
        where (Entry meta post) = x
    readJSON x = Ok $ Entry meta post
        where meta = peel x "MetaData"
              post = peel x "Posting"

instance JSON (MetaData) where
    showJSON (Subject  x) = JSObject $ toJSObject [ ("Subject",  JSString $ toJSString $ x) ]
    showJSON (Date     x) = JSObject $ toJSObject [ ("Date",     JSString $ toJSString $ x) ]
    showJSON (To x) = JSObject $ toJSObject [ ("To", JSArray $ map (JSString . toJSString) $ x) ]
    showJSON (From   x) = JSObject $ toJSObject [ ("From",   JSString $ toJSString $ x) ]
    readJSON x =  Ok $ case (fst.head $ jsObj) of
                           "Subject"  -> Subject  (fromOK.readJSON.snd.head $ jsObj)
                           "Date"     -> Date     (fromOK.readJSON.snd.head $ jsObj)
                           "To" -> To (fromOK.readJSON.snd.head $ jsObj)
                           "From"   -> From   (fromOK.readJSON.snd.head $ jsObj)
        where jsObj = fromOK.jsonObject $ x
              fromOK (Ok x)    = x
              fromOK (Error x) = error x
            
instance JSON (BlogText) where
    showJSON (PureT x)  = JSObject $ toJSObject [ ("PureT", JSString $ toJSString $ x) ]
    showJSON (PureC x)  = JSObject $ toJSObject [ ("PureC", showJSON x) ]
    showJSON (MixT x y) = JSObject $ toJSObject [ ("MixT", JSArray [ JSString $ toJSString $ x, showJSON y] ) ]
    showJSON (MixC x y) = JSObject $ toJSObject [ ("MixC", JSArray [ showJSON x, showJSON y ] ) ]
    showJSON (Empty)    = JSObject $ toJSObject [ ("Empty", JSNull) ]
    readJSON x = Ok $ case (fst.head $ jsObj) of
                          "PureT" -> PureT (fromOK.readJSON.snd.head $ jsObj)
                          "PureC" -> PureC (fromOK.readJSON.snd.head $ jsObj) 
                          "MixT"  -> MixT  (fromOK.jsonString.head.fromOK.readJSONs.snd.head $ jsObj) (fromOK.readJSON.head.(drop 1).fromOK.readJSONs.snd.head $ jsObj) 
                          "MixC"  -> MixC  (fromOK.readJSON.head.fromOK.readJSONs.snd.head $ jsObj) (fromOK.readJSON.head.(drop 1).fromOK.readJSONs.snd.head $ jsObj) 
                          "Empty" -> Empty
        where jsObj = fromOK.jsonObject $ x
              fromOK (Ok x)    = x
              fromOK (Error x) = error x

instance JSON (Command) where
    showJSON (Bold      x) = JSObject $ toJSObject [ ("Bold", showJSON x) ]
    showJSON (Italic    x) = JSObject $ toJSObject [ ("Italic", showJSON x) ]
    showJSON (Underline x) = JSObject $ toJSObject [ ("Underline", showJSON x) ]
    showJSON (Strike    x) = JSObject $ toJSObject [ ("Strike", showJSON x) ]
    showJSON (Link x y   ) = JSObject $ toJSObject [ ("Link", JSArray [ showJSON x, showJSON y ] ) ]
    showJSON (None)        = JSObject $ toJSObject [ ("None", JSNull) ]
    readJSON x = Ok $ case (fst.head $ jsObj) of
                          "Bold"      -> Bold      (fromOK.readJSON.snd.head $ jsObj)
                          "Italic"    -> Italic    (fromOK.readJSON.snd.head $ jsObj)
                          "Underline" -> Underline (fromOK.readJSON.snd.head $ jsObj)
                          "Strike"    -> Strike    (fromOK.readJSON.snd.head $ jsObj)
                          "Link"      -> Link      (fromOK.readJSON.snd.head $ jsObj) (fromOK.readJSON.snd.head.(drop 1) $ jsObj)
                          "None"      -> None
        where jsObj = fromOK.jsonObject $ x
              fromOK (Ok x)    = x
              fromOK (Error x) = error x
           

pText = many1 $ noneOf "\\{}[]%"
pLine = many  $ noneOf "\\{}[]%\n"

pEmptyLine = do spaces
                many1 (char '\n')

pCmdName = many1 $ noneOf "\\{}[]% "
pCmdConf = between (char '[') (char ']') pText
pCmdBody = between (char '{') (char '}') pBlogText


pBlogEntry = do meta  <- many1 (choice [pMetaSub, pMetaDte, pMetaCat, pMetaAut])
                entry <- pBlogText
                return (Entry meta entry)

pBlogText =
    do x <- try ( do txt <- pText
                     x   <- pBlogText
                     return (MixT txt x)
                )
            <|> 
            try ( do cmd <- pCommand
                     x   <- pBlogText
                     return (MixC cmd x)
                )
            <|> ( do cmd <- pCommand
                     return (PureC cmd)
                )
            <|> ( do txt <- pText 
                     return (PureT txt)
                )
            <|> ( do eof
                     return (Empty)
                )
       return x

pCommand =
    do char '\\'
       x <- try ( do name <- pCmdName
                     body <- pCmdBody
                     case name of 
                         "bold"      -> return (Bold body)
                         "italic"    -> return (Italic body)
                         "underline" -> return (Underline body)
                         "strike"    -> return (Strike body)
                         otherwise   -> return None
                )
            <|> ( do name <- pCmdName
                     conf <- pCmdConf
                     body <- pCmdBody
                     case name of 
                         "link"      -> return (Link conf body)
                         otherwise   -> return None
                )       
       return x

pMetaSub = do string "Subject"
              spaces
              char ':'
              spaces
              sub <- pLine
              many1 (char '\n')
              return (Subject sub)

pMetaDte = do string "Date"
              spaces
              char ':'
              spaces
              dte <- pLine
              many1 (char '\n')
              return (Date dte)

pMetaCat = do string "To"
              spaces
              char ':'
              spaces
              cat <- pLine `sepBy1` (char ',')
              many1 (char '\n')
              return (To cat)

pMetaAut = do string "From"
              spaces
              char ':'
              spaces
              aut <- pLine
              many1 (char '\n')
              return (From aut)
