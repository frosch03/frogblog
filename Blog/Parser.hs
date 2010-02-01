module Blog.Parser where

import Blog.DataDefinition
import Text.ParserCombinators.Parsec

-- | rspaces is another version of spaces with the difference, that 
--   the newline-char is not allowed as space-char
rspaces = skipMany (oneOf " \t\v\f\r")

pText           = many1 $ noneOf "\\{}[]%"
pLine           = many1 $ noneOf "\\{}[]%\n"

pString2Line     = many1 $ noneOf "\\{}[]%\n\""
pString2Category = do result <- many  $ noneOf ",\\{}[]%\n\" "
                      rspaces
                      return result

pCategory = do result <- many  $ noneOf ",\\{}[]%\n "
               rspaces
               return result

pEmptyLine = do spaces
                many1 (char '\n')

pCmdName = many1 $ noneOf "\\{}[]% "
pCmdConf = between (char '[') (char ']') pText
pCmdBody = between (char '{') (char '}') pBlogText

pMeta = do many1 (choice [pMetaSub, pMetaDate, pMetaTo, pMetaFrom])

pBlogEntry = do meta  <- pMeta
                entry <- pBlogText
                return (Entry meta entry)

pBlogText =
    do x <- try ( do txt <- pLine
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
            <|> ( do txt <- pLine
                     return (PureT txt)
                )
            <|> ( do eof
                     return (Empty)
                )
       return x

pCommand =
        try ( do char '\n'
                 return Break
            ) 
        <|>
        do char '\\'
           x <- try ( do result <- (pBlock "code")
                         return (Code result)
                    )
                <|> 
                try ( do name <- pCmdName
                         body <- pCmdBody
                         case name of
                             "bold"      -> return (Bold body)
                             "italic"    -> return (Italic body)
                             "underline" -> return (Underline body)
                             "strike"    -> return (Strike body)
                             "section"   -> return (Section body)
                             otherwise   -> return None
                    )
                <|>
                    ( do name <- pCmdName
                         conf <- pCmdConf
                         body <- pCmdBody
                         case name of
                             "link"      -> return (Link conf body)
                             otherwise   -> return None
                    )
           return x

pBlock blockType = 
    do string "begin"
       between (char '{') (char '}') (string blockType)
       spaces
       result <- manyTill anyChar (try $ pBlockEnd blockType)
       return result

pBlockEnd blockType =
    do string "\\end"
       between (char '{') (char '}') (string blockType)
       rspaces


pMetaSub  = do string "Subject"
               spaces
               char ':'
               spaces
               sub <- pLine
               (char '\n')
               return (Subject sub)

pMetaDate = do string "Date"
               spaces
               char ':'
               spaces
               date <- pLine
               (char '\n')
               return (Date date)

pMetaTo   = do string "To"
               spaces
               char ':'
               spaces
               to <- pCategory `sepBy1` do {(char ','); rspaces}
               (char '\n')
               return (To to)

pMetaFrom = do string "From"
               spaces
               char ':'
               spaces
               from <- pLine
               (char '\n')
               return (From from)



pQuotedString = between (char '"') (char '"')

pString2Sub = do string "Subject"
                 spaces
                 sub <- pQuotedString pString2Line
                 return (Subject sub)

pString2Date = do string "Date"
                  spaces
                  date <- pQuotedString pString2Line
                  return (Date date)

pString2From = do string "From"
                  spaces
                  from <- pQuotedString pString2Line
                  return (From from)

pString2To   = do string "To"
                  spaces
                  to <- pQuotedString (pString2Category `sepBy1` do {(char ','); rspaces})
                  return (To to)

instance Read (MetaData) where
    readsPrec p s = case parse (choice [pString2Sub, pString2Date, pString2To, pString2From]) "" s of
                        Left  _ -> []
                        Right x -> [(x, "")]

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


