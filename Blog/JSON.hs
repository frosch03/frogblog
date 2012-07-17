module Blog.JSON 
    ()
where

-- Extern 
import Text.JSON
import Database.CouchDB.JSON ( jsonField
                             , jsonObject
                             , jsonString
                             )

-- Intern
import Blog.Definition
import Blog.Auxiliary (dropOK)
import Auxiliary (firstUp)

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
    showJSON (To       x) = JSObject $ toJSObject [ ("To",       JSArray $ map (JSString . toJSString) $ (firstUp x)) ]
    showJSON (From     x) = JSObject $ toJSObject [ ("From",     JSString $ toJSString $ x) ]
    readJSON x =  Ok $ case (fst.head $ jsObj) of
                           "Subject"  -> Subject  (fromOK.readJSON.snd.head $ jsObj)
                           "Date"     -> Date     (fromOK.readJSON.snd.head $ jsObj)
                           "To"       -> To       (map (fromOK.jsonString) ((fromOK.readJSONs.snd.head $ jsObj) :: [JSValue]))
                           "From"     -> From     (fromOK.readJSON.snd.head $ jsObj)
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
    showJSON (Break)          = JSObject $ toJSObject [ ("Break", JSNull) ]
    showJSON (Block        x) = JSObject $ toJSObject [ ("Block", showJSON x) ]
    showJSON (Bold         x) = JSObject $ toJSObject [ ("Bold", showJSON x) ]
    showJSON (Italic       x) = JSObject $ toJSObject [ ("Italic", showJSON x) ]
    showJSON (Underline    x) = JSObject $ toJSObject [ ("Underline", showJSON x) ]
    showJSON (Strike       x) = JSObject $ toJSObject [ ("Strike", showJSON x) ]
    showJSON (Section      x) = JSObject $ toJSObject [ ("Section", showJSON x) ]
    showJSON (Link x y      ) = JSObject $ toJSObject [ ("Link", JSArray [ showJSON x, showJSON y ] ) ]
    showJSON (CommandBlock x) = JSObject $ toJSObject [ ("CommandBlock", showJSON x) ]
    showJSON (Code         x) = JSObject $ toJSObject [ ("Code", showJSON x) ]
    showJSON (Itemize      x) = JSObject $ toJSObject [ ("Itemize", showJSON x) ]
    showJSON (None)           = JSObject $ toJSObject [ ("None", JSNull) ]
    readJSON x = Ok $ case (fst.head $ jsObj) of
                          "Break"        -> Break
                          "Block"        -> Block        (fromOK.readJSON.snd.head $ jsObj)
                          "Bold"         -> Bold         (fromOK.readJSON.snd.head $ jsObj)
                          "Italic"       -> Italic       (fromOK.readJSON.snd.head $ jsObj)
                          "Underline"    -> Underline    (fromOK.readJSON.snd.head $ jsObj)
                          "Strike"       -> Strike       (fromOK.readJSON.snd.head $ jsObj)
                          "Section"      -> Section      (fromOK.readJSON.snd.head $ jsObj)
                          "Link"         -> Link         (fromOK.jsonString.head.fromOK.readJSONs.snd.head $ jsObj) (fromOK.readJSON.head.(drop 1).fromOK.readJSONs.snd.head $ jsObj)
                          "CommandBlock" -> CommandBlock (map (fromOK.readJSON) (fromOK.readJSONs.snd.head $ jsObj))
                          "Code"         -> Code         (fromOK.readJSON.snd.head $ jsObj)
                          "Itemize"      -> Itemize      (map (fromOK.readJSON) (fromOK.readJSONs.snd.head $ jsObj))
                          "None"         -> None
        where jsObj = fromOK.jsonObject $ x
              fromOK (Ok x)    = x
              fromOK (Error x) = error x


peel js key = (fromOK.(jsonField key)) $ fromOK.jsonObject $ js
    where fromOK (Ok x)    = x 
          fromOK (Error x) = error x

