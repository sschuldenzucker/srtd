{-# LANGUAGE DeriveGeneric #-}

-- | Helpers for JSON encoding. We put them here not to pollute the namespace. Should be imported qualified.
module ModelJSON where

import Attr (Attr, EID)
import Data.Aeson
import GHC.Generics

data Model = Model
  { forest :: [Tree]
  }
  deriving (Generic)

data Tree = Tree
  { id :: EID,
    attr :: Attr,
    children :: [Tree]
  }
  deriving (Generic)

instance ToJSON Tree where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Tree

instance ToJSON Model where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Model
