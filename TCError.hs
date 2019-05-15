module TCError where

import Control.Monad.Writer

data Error = Error String deriving (Show)
type TC = Writer [Error]
