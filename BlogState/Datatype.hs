module BlogState.Datatype
    ( Date(..)
    , BlogState(..)
    )
where

import Filter (Filter(..))

type Day   = Int
type Month = Int
type Year  = Int

newtype Date = D (Year, Month, Day)

data BlogState = BS Date Filter
