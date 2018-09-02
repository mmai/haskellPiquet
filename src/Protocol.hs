{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Protocol ( Msg(..) ) where

import Data.Text (Text)
import Data.Aeson hiding ((.=))
import Data.Aeson.Casing
import Data.Binary hiding (get)
import GHC.Generics


import Cards

data Msg = SetCombination Hand 
         | ChangeName Text 
         deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

