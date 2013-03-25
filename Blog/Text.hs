module Blog.Text 
    ( TEXT(..)
    -- , fromLines
    )
where

-- Extern
import Text.Pandoc

-- Intern
import Blog.Definition
-- import Blog.Auxiliary (textLink)
import Config

class TEXT a where
    toText :: a -> String

instance TEXT (BlogEntry) where
  toText (Entry _ post) = writePlain def post

