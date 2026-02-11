module Srtd.Components.CompilingTextEntryBanana where

-- only helpers

import Data.Text (Text)
import Srtd.BananaComponent
import Srtd.Component
import Srtd.Components.CompilingTextEntry (
  CompiledWithSource (..),
  ConfirmType (..),
  compileWithSource,
 )

type MyContinueType c = Maybe (CompiledWithSource c)

type MyConfirmType c = (CompiledWithSource c, ConfirmType)

data CompilingTextEntryBanana c = CompilingTextEntryBanana
  {ctebComponent :: AppComponentBanana (MyContinueType c) (MyConfirmType c)}

-- SOMEDAY instead of the class, I could also invert and have a master type
-- data SomeAppComponentBanana a b c = SomeAppComponentBanana {sacbComponent :: AppComponentBanana a b; sacbExtra :: c}
instance HasAppComponentBanana (CompilingTextEntryBanana c) (MyContinueType c) (MyConfirmType c) where
  getAppComponentBanana = ctebComponent

-- TODO WIP
mkCompilingTextEntryBanana ::
  (Text -> Maybe c) ->
  Text ->
  AppResourceName ->
  BananaMaker (CompilingTextEntryBanana c)
-- TODO WIP understand compat with keymap, specifically the zipper
mkCompilingTextEntryBanana f s rname actb eBrickEvent = _todo
