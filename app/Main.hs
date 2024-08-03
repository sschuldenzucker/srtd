module Main(main) where

import Brick.Main
import Control.Monad (void)
import Brick.Themes
import Graphics.Vty (white, blue, green, yellow, black, magenta, Event(..), Key(..))
import Brick (on)
import qualified Brick.AttrMap
import Brick.Widgets.Core (str)
import Brick.Types ( Widget, BrickEvent(..) )
import Brick.Types (EventM)
import Control.Monad.State (modify)


-- main :: IO ()
-- main = putStrLn "Hello, Haskell!" >> ffun 1 2

main :: IO()
main = void $ defaultMain app 1

myAttrMap :: Brick.AttrMap.AttrMap
myAttrMap = themeToAttrMap $ newTheme (green `on` black) []

ui :: Int -> Widget n
ui i = str (show i)

myHandleEvent :: BrickEvent () e -> EventM () Int ()
myHandleEvent ev = case ev of
  (VtyEvent (EvKey (KChar 'q') [])) -> halt
  (VtyEvent (EvKey (KChar '+') [])) -> modify (+1)
  (VtyEvent (EvKey (KChar '-') [])) -> modify (\x -> x-1)
  _ -> return ()

app :: App Int e ()
app = App
  { appDraw = \s -> [ui s]
  , appHandleEvent = myHandleEvent
  , appStartEvent = return ()
  , appAttrMap = const myAttrMap
  , appChooseCursor = neverShowCursor
}

