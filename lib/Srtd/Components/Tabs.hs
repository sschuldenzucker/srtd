{-| A set of tabs, with a tab bar.

Import qualified.
-}
module Srtd.Components.Tabs (
  -- * Types
  Tabs (..),
  TabsEvent (..),

  -- * Construction
  make,

  -- * Access
  isEmpty,
  activeRname,
  activeTabL,

  -- * Modification
  pushTab,
  popTab,
  nextTab,
  prevTab,
  swapTabNext,
  swapTabPrev,
) where

import Brick
import Control.Arrow (second)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState, runStateT)
import Control.Monad.Writer (lift, tell)
import Data.List (intersperse)
import Data.List.Zipper qualified as LZ
import Data.Text (Text)
import Lens.Micro.Platform
import Srtd.AppAttr qualified as AppAttr
import Srtd.Component
import Srtd.Keymap (KeyDesc (..))
import Srtd.Log (Priority (..), glogL)
import Srtd.Util (slipl, tell1)
import Srtd.Util.ListZipper

-- | Tabs of type `s`. `s` is often `SomeAppComponent`.
--
-- We don't technically _require_ tabs to be nonempty, but you probably want this. This is why we
-- return Confirmed or Canceled if an event closed the last tab.
data Tabs s = Tabs
  { tTabs :: LZ.Zipper (AppResourceName, s)
  , tNextTabID :: Int
  , tRname :: AppResourceName
  , tTitle :: Text
  }

suffixLenses ''Tabs

make :: Int -> Text -> [AppResourceName -> s] -> AppResourceName -> Tabs s
make initTabID title initMks rname =
  Tabs
    { tTabs = LZ.fromList initTabsWithRnames
    , tNextTabID = nextTabID
    , tRname = rname
    , tTitle = title
    }
 where
  (initTabsWithRnames, nextTabID) = slipl foldr ([], initTabID) initMks $ \mk (rss, i) ->
    let tabRname = mkTabRname rname i
     in ((tabRname, mk tabRname) : rss, i + 1)

mkTabRname :: AppResourceName -> Int -> AppResourceName
mkTabRname rname i = rname <> AppResourceName [NamedAppResource "tab" i]

isEmpty :: Tabs s -> Bool
isEmpty = LZ.emptyp . tTabs

-- | Focused resource name, or Nothing if empty.
activeRname :: Tabs s -> Maybe AppResourceName
activeRname ts =
  let tabs = tTabs ts
   in if
        | LZ.emptyp tabs -> Nothing
        | otherwise -> Just . fst $ LZ.cursor tabs

-- | Focus on the active tab. Partial, throws an error if there is no active tab!
--
-- SOMEDAY I guess this should then be a prism / loupe / mirror / whatever.
activeTabL :: Lens' (Tabs s) s
-- There's probably some clever way to do this but idk. It's also trivial rn.
activeTabL = lens getter setter
 where
  getter Tabs {tTabs} = snd $ LZ.cursor tTabs
  setter state@(Tabs {tTabs}) t' = state {tTabs = lzModify (second $ const t') tTabs}

newTabRname :: (MonadState (Tabs s) m) => m AppResourceName
newTabRname = do
  ret <- mkTabRname <$> (gets tRname) <*> (gets tNextTabID)
  tNextTabIDL += 1
  return ret

pushTab :: (AppResourceName -> s) -> ComponentEventM (Tabs s) AppResourceName
pushTab mk = do
  tabRname <- newTabRname
  tTabsL %= LZ.insert (tabRname, mk tabRname) . LZ.right
  tell1 (TabPushed tabRname)
  return tabRname

-- | Pop a tab and return whether the tabs list is now empty.
-- We do it like this b/c this usually needs to be handled by the caller.
popTab :: ComponentEventM (Tabs s) Bool
popTab = do
  tTabsL %= lzDeleteLeft
  tell1 TabPopped
  LZ.emptyp <$> gets tTabs

nextTab, prevTab, swapTabNext, swapTabPrev :: ComponentEventM (Tabs s) ()
nextTab = tTabsL %= lzCircRight
prevTab = tTabsL %= lzCircLeft
swapTabNext = tTabsL %= lzSwapRightCirc
swapTabPrev = tTabsL %= lzSwapRightCirc

-- | Execute an action in each tab and collect results.
callIntoEachTab :: ComponentEventM s a -> ComponentEventM (Tabs s) (LZ.Zipper a)
callIntoEachTab act = do
  tabs <- gets tTabs
  sress <- lzForM tabs $ \(rname, s) -> do
    (s', (res, events)) <- nestComponentEventM s act
    tell $ (TabEvent rname) <$> events
    return ((rname, s'), res)
  tTabsL .= (fst <$> sress)
  return $ snd <$> sress

-- | Execute an action returning 'AppEventReturn' in each tab (usually event handling), remove
-- confirmed/canceled tabs, raise 'TabConfirmed' events, and return with a value on our own:
-- Canceled if all tabs have canceled, Confirmed if all have canceled or confirmed and at least one
-- has confirmed, and Continue otherwise. This matches a common use case, where Canceled is returned
-- on error.
handleEachTab ::
  ComponentEventM' s -> ComponentEventM' (Tabs s)
handleEachTab act = do
  tabs <- gets tTabs
  (mss', atLeastOneConfirmed) <- flip runStateT False $ lzForM tabs $ \(rname, s) -> do
    (s', (res, events)) <- lift $ nestComponentEventM s act
    tell $ (TabEvent rname) <$> events
    case res of
      Continue -> return $ Just (rname, s')
      Confirmed x -> do
        put True
        tell1 $ TabConfirmed rname x
        return Nothing
      Canceled -> return Nothing
  tTabsL .= lzCatMaybesLeftNonEmpty mss'
  ise <- gets isEmpty
  return $
    if
      | ise && atLeastOneConfirmed -> Confirmed ()
      | ise -> Canceled
      | otherwise -> Continue

-- | Call into the active tab, if any, or return Nothing.
callIntoActiveTabMaybe ::
  ComponentEventM s a -> ComponentEventM (Tabs s) (Maybe (AppResourceName, a))
callIntoActiveTabMaybe act = do
  mtrname <- gets activeRname
  case mtrname of
    Nothing -> return Nothing
    Just trname ->
      let h = tell1 . TabEvent trname
       in callIntoComponentEventM activeTabL h act <&> \ret -> Just (trname, ret)

-- | Execute an action returning 'AppEventReturn' in the active tab. If there is no active tab, we
-- return Canceled. If there is one, return Continue unless the last tab just closed. In that case,
-- return Canceled if the last tab canceled and Confirm if it confirmed, like in 'handleEachTab'
handleActiveTab :: ComponentEventM' s -> ComponentEventM' (Tabs s)
handleActiveTab act =
  callIntoActiveTabMaybe act >>= \case
    -- We assume here that the action _should really_ be done, and when there's nothing to handle it,
    -- that's an error.
    Nothing -> return Canceled
    Just (_, Continue) -> return Continue
    Just (_, Canceled) -> do
      ise <- popTab
      return $ if ise then Canceled else Continue
    Just (trname, Confirmed x) -> do
      tell1 $ TabConfirmed trname x
      ise <- popTab
      return $ if ise then Confirmed () else Continue

renderTabBar :: (AppComponent s) => LZ.Zipper (AppResourceName, s) -> Widget AppResourceName
renderTabBar tabs =
  withDefAttr AppAttr.tab_bar $
    let (front, cur, back) = lzSplit3 tabs
     in hBox $
          intersperse (txt "|") $
            map (uncurry (renderTabTitle False)) front
              ++ [uncurry (renderTabTitle True) cur]
              ++ map (uncurry (renderTabTitle False)) back
              ++ [padLeft Max (str " ")]
 where
  renderTabTitle :: (AppComponent c) => Bool -> AppResourceName -> c -> Widget AppResourceName
  renderTabTitle sel rname c =
    let theAttrName =
          (if sel then AppAttr.tab_bar <> attrName "selected" else AppAttr.tab_bar)
            <> attrName "tab_title"
     in -- TODO WIP nope gotta flag that it's the tab title, not the component itself.
        clickable rname . withAttr theAttrName . padLeftRight 1 . hLimit 25 $ txt (componentTitle c)

data TabsEvent s
  = TabPushed AppResourceName
  | TabPopped
  | TabEvent AppResourceName (Event s)
  | TabConfirmed AppResourceName (Return s)

instance (AppComponent s) => AppComponent (Tabs s) where
  type Return (Tabs s) = ()
  type Event (Tabs s) = TabsEvent s

  renderComponentWithOverlays s =
    let curTab = snd . LZ.cursor . tTabs $ s
        (curTabW, ovls) = renderComponentWithOverlays curTab
     in (hBox [renderTabBar (tTabs s), curTabW], ovls)

  handleEvent ev = case ev of
    AppEvent _ -> handleEachTab $ handleEvent ev
    VtyEvent _ -> handleActiveTab $ handleEvent ev
    MouseDown rname _button _mods _loc -> routeMouse rname
    MouseUp rname _mbtn _loc -> routeMouse rname
   where
    routeMouse rname = dispatchChildRName "Tabs" tRname rname $ \errmsg rntail -> case rntail of
      -- We don't check which tab was clicked into b/c it only _can_ be the active one, b/c no other one is visible.
      NamedAppResource "tab" _i : _ -> handleActiveTab $ handleEvent ev
      -- TODO tab title
      _ -> do
        liftIO $ glogL WARNING errmsg
        return Continue

  componentTitle = tTitle

  componentKeyDesc ts
    | isEmpty ts = KeyDesc (componentTitle ts) True []
    | otherwise = componentKeyDesc (ts ^. activeTabL)
