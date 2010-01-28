module Parser where

import Text.ParserCombinators.Parsec

type Text = [String]

data Command  = Bold      Content
              | Italic    Content
              | Underline Content
              | Strike    Content
              | None

data Content = T Text    Content
             | C Command Content
             | End

p_text = 
    do x <- many1 $ noneOf "\\{}[]%"
       return (T [x] End) 

p_group = 
    do char '{'
       x <- p_doc
       char '}'
       return x

p_config = 
    do char '['
       x <- p_doc
       char ']'
       return x

p_comment =
    do char '%'
       x <- manyTill anyChar newline
       return []

p_commandToken =
    do char '\\'
       x <- many1 $ noneOf "\\{}[]% "
       return x

p_commandConfig =
    do try ( do p_comment
           )
       <|> ( do x <- p_config
                return x
           )

p_commandGroup =
    do try ( do p_comment 
           )
       <|> ( do x <- p_group
                return x
           )

p_command = 
    do cmdT <- p_commandToken
       cmdG <- try ( do cmdC <- p_commandConfig
                        cmdG <- p_commandGroup
                        return ("" ++ cmdC ++ "\"" ++ cmdG)
                   )
               <|> ( do cmdG <- p_commandGroup
                        return (cmdG)
                   )
       return ((p_exactCMD cmdT) ++ cmdG ++ ">>")

p_exactCMD = do
           try ( do string "bold"
                    x <- between (char '{') (char '}') p_doc
                    return (Bold x))
       <|> try ( do string "italic"
                    x <- between (char '{') (char '}') p_doc
                    return (Italic x))
       <|> try ( do string "underline"
                    x <- between (char '{') (char '}') p_doc
                    return (Underline x))
       <|> try ( do string "strike"
                    x <- between (char '{') (char '}') p_doc
                    return (Strike x))

p_doc = 
    do     try ( p_exactCMD )
       <|> try ( p_comment )
--     <|> try ( p_config )
--     <|> try ( p_group )
       <|> try ( p_text )

-- text := RegExp(r"[^\\\{\}\[\]%]+")
--
-- group   := "{" doc "}"
-- config  := "[" doc "]"
-- comment := RegExp(r'%.*\n')
--
-- commandToken := RegExp(r"\\\\?[^\\\{\}\[\]%\s]*")
-- 
-- commandConfig := comment? config
-- commandGroup  := comment? group
--
-- command := commandToken commandConfig? commandGroup*
--
-- doc := ( command | comment | config | group | text )*
