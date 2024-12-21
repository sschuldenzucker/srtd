{-# LANGUAGE TemplateHaskell #-}

-- SOMEDAY This is currently unused. One reason is that we have ambiguous type variable issues (?!?) without function signatures and function signatures are kinda a pain to do (we can't just put them into the d splice) and we also need to construct the class name etc.

module Srtd.Attr.TH where

import Control.Monad (forM)
import Data.Char (isLower, toUpper)
import Language.Haskell.TH

-- | field -> getField
nameGetCapitalize :: String -> String
nameGetCapitalize s = "get" ++ capFirst s
 where
  capFirst [] = []
  capFirst (c : cs) = toUpper c : cs

-- | mrField -> getField
nameGetFirstCapital :: String -> String
nameGetFirstCapital s = "get" ++ dropWhile isLower s

-- |  Given the name of a record type (i.e., a `data` type with a single constructor and named
-- fields) R, this function expects a class like this:
--
-- ```
-- class HasR where getR :: a -> R
-- ```
--
-- It then, for each field f, generates a universal getter function `getF :: (HasR a) => a -> (type
-- of f)`. The second parameter can be used to configure how field names are translated into getter
-- function names.
--
-- SOMEDAY generate the class and its trivial instance. But I can't be bothered rn.
mkUniversalGetters :: Name -> (String -> String) -> Q [Dec]
mkUniversalGetters recordTypeName namer = do
  -- Generate the class name (e.g., `HasMyRecord` for `MyRecord`)
  let structGetterName = mkName $ namer $ nameBase recordTypeName

  -- Reify the record type to extract its fields
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify recordTypeName

  -- Generate universal getters for all fields
  fmap concat $ forM fields $ \(fieldName, _, _) -> do
    let functionName = mkName $ "g" ++ nameBase fieldName
    [d|
      $(varP functionName) = $(varE fieldName) . $(varE structGetterName)
      |]
