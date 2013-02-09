{-# LANGUAGE NoMonomorphismRestriction #-}

module Blog.Parser 
where

-- Extern
import Text.ParserCombinators.Parsec
import Text.Pandoc as P
--import Data.Set (insert)

-- Intern
import Blog.Definition

-- Blog-Parser
--------------

pBlogEntry = 
    do meta  <- pMeta
       entry <- pBlogText -- pPandoc
       return (Entry meta (pPandoc entry))

pMeta = 
    do many1 (choice [pMetaSub, pMetaDate, pMetaTo, pMetaFrom])

pPandoc = P.readMarkdown P.def -- def'
--    where def' = P.def { readerExtensions = insert Ext_tex_math_dollars (readerExtensions P.def) }          


pBlogText = manyTill anyChar eof

-- pBlogText =
--     do x <- try ( do cmd <- many1 (pCommandLine)
--                      return (PureC $ CommandBlock cmd)
--                 )
--             <|>
--             try ( do txt <- pLine
--                      x   <- pBlogText
--                      return (MixT txt x)
--                 )
--             <|>
--             try ( do cmd <- pCommand
--                      x   <- pBlogText
--                      return (MixC cmd x)
--                 )
--             <|> ( do cmd <- pCommand
--                      return (PureC cmd)
--                 )
--             <|> ( do txt <- pLine
--                      return (PureT txt)
--                 )
--             <|> ( do eof
--                      return (Empty)
--                 )
--        return x

-- pCommand =
--     try ( do char '\n'
--              char '\n'
--              body <- pBlogText
--              return (Block body)
--         )
--     <|>
--     try ( do result <- many1 (pCommandLine)
--              return (CommandBlock result)
--         )
--     <|>
--     try ( do char '\n'
--              return Break
--         )
--     <|>
--     try ( do result <- (pItemize)
--              return (Itemize result)
--         )
--     <|>
--     try ( do x <- try ( do result <- (pCodeBlock)
--                            return (Code result)
--                       )
--                   <|>
--                   try ( do char '\\'
--                            name <- pCmdName
--                            body <- pCmdBody
--                            case name of
--                                "bold"      -> return (Bold body)
--                                "italic"    -> return (Italic body)
--                                "underline" -> return (Underline body)
--                                "strike"    -> return (Strike body)
--                                "section"   -> return (Section body)
--                                otherwise   -> return None
--                       )
--                   <|>
--                   try ( do char '\\'
--                            name <- pCmdName
--                            conf <- pCmdConf
--                            body <- pCmdBody
--                            case name of
--                                "link"      -> return (Link conf body)
--                                otherwise   -> return None
--                       )
--              return x
--         )

-- Instance definitions
-----------------------
instance Read (MetaData) where
    readsPrec p s = case parse (choice [pString2Sub, pString2Date, pString2To, pString2From]) "" s of
                        Left  _ -> error $ "error while parsing MetaData"
                        Right x -> [(x, "")]

instance Read (BlogEntry) where
    readsPrec p s = case parse pBlogEntry "" s of
                        Left  _ -> error $ "error while parsing BlogEntry"
                        Right x -> [(x, "")]

-- instance Read (BlogText) where
--     readsPrec p s = case parse pBlogText "" s of
--                         Left  _ -> error $ "error while parsing BlogText"
--                         Right x -> [(x,"")]

-- instance Read (Pandoc) where
--     readsPrec p s = case pPandoc "" s of
--                         Left  _ -> error $ "error parsing Markdown (with pandoc)"
--                         Right x -> [(x,"")]

-- instance Read (Command) where
--     readsPrec p s = case parse pCommand "" s of
--                         Left  _ -> []
--                         Right x -> [(x, "error while parsing Command")]

-- Supporting parsers
---------------------
-- | rspaces is another version of spaces with the difference, that 
--   the newline-char is not allowed as space-char
rspaces          = skipMany (oneOf " \t\v\f\r")

pText            = many1 $ noneOf "\\{}[]%"
pLine            = many1 $ noneOf "\\{}[]%\n"
pCommandLine     = do string "> "
                      result <- many $ noneOf "\n"
                      char '\n'
                      return (result)

pString2Line     = many1 $ noneOf "\\{}[]%\n\""
pString2Category = do result <- many  $ noneOf ",\\{}[]%\n\" "
                      rspaces
                      return result

pCategory        = do result <- many  $ noneOf ",\\{}[]%\n"
                      rspaces
                      return result

pCmdName         = many1 $ noneOf "\\{}[]% "
pCmdConf         = between (char '[') (char ']') pLine
-- pCmdBody         = between (char '{') (char '}') pBlogText


pItem' = 
    do spaces
       string "\\item"
       spaces

-- pItem = 
--     do pItem'
--        sepBy1 pBlogText pItem'

-- pItemize = 
--     do pBlockBegin "itemize"
--        tmp    <- manyTill anyChar (try $ pBlockEnd "itemize")
--        case (runParser pItem () "" tmp) of
--           Left _   -> error "error while parsing itemize environment"
--           Right xs -> return xs

pCodeBlock = 
    do pBlockBegin "code"
       result <- manyTill anyChar (try $ pBlockEnd "code")
       return result
       
pBlockBegin blockType =
    do string "\\begin"
       between (char '{') (char '}') (string blockType)
       spaces

pBlockEnd blockType =
    do spaces
       string "\\end"
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
