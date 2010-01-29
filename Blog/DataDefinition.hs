module Blog.DataDefinition where

type URL        = String
type Text       = String
type Category   = String
type Author     = String

data BlogEntry
    = Entry [MetaData] BlogText
    deriving (Show)

data MetaData
    = Subject  String
    | Date     String
    | To      [String]
    | From     String
    deriving (Show)


data BlogText
    = PureT Text
    | PureC Command
    | MixT  Text BlogText
    | MixC  Command BlogText
    | Empty
    deriving (Show)


data Command
    = Break
    | Bold      BlogText
    | Italic    BlogText
    | Underline BlogText
    | Strike    BlogText
    | Link URL  BlogText
    | None
    deriving (Show)
