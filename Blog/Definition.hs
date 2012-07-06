module Blog.Definition 
    ( BlogEntry(..)
    , MetaData(..)
    , BlogText(..)
    , Command(..)
    , Category
    , Author
    )
where


type URL        = String
type Text       = String
type Category   = String
type Author     = String

data BlogEntry
    = Entry [MetaData] BlogText
    | Short [MetaData] BlogText
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
    | Block       BlogText
    | Bold        BlogText
    | Italic      BlogText
    | Underline   BlogText
    | Strike      BlogText
    | Section     BlogText
    | Link URL    BlogText
    | Itemize    [BlogText]
    | Code        String
    | CommandLine String
    | None
    deriving (Show)
