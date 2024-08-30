-- | Alignment helpers for brick, to get e.g. transparent ("layer") bottom-right alignment.
--
-- Kinda shocking it doesn't have that tbh.
--
-- NB if you want non-transparent (non-"layer") alignment, this can simply be achieved using
-- `padLeft Max` etc. That is probably the better choice in most situations (and less flakey with
-- colors), *except* overlay windows.
module Srtd.Alignment where

import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty
  ( imageHeight,
    imageWidth,
    translateX,
    translateY,
  )
import Lens.Micro.Platform (to, (&), (.~), (^.))

-- | Counterpart to hCenterLayer that aligns right.
--
-- Mostly copied from `hCenterLayer`
hAlignRightLayer :: Widget n -> Widget n
hAlignRightLayer p =
  Widget Greedy (vSize p) $ do
    result <- render p
    c <- getContext
    let rWidth = result ^. imageL . to imageWidth
        -- leftPaddingAmount = max 0 $ (c ^. availWidthL - rWidth) `div` 2
        leftPaddingAmount = max 0 $ (c ^. availWidthL - rWidth)
        paddedImage = translateX leftPaddingAmount $ result ^. imageL
        off = Location (leftPaddingAmount, 0)
    if leftPaddingAmount == 0
      then
        return result
      else
        return $
          addResultOffset off $
            result & imageL .~ paddedImage

-- | Counterpart to vCenterLayer that aligns to the bottom.
--
-- Mostly copied from `vCenterLayer`
vAlignBottomLayer :: Widget n -> Widget n
vAlignBottomLayer p =
  Widget (hSize p) Greedy $ do
    result <- render p
    c <- getContext
    let rHeight = result ^. imageL . to imageHeight
        -- topPaddingAmount = max 0 $ (c ^. availHeightL - rHeight) `div` 2
        -- TODO WIP Review, then use it for the help box
        topPaddingAmount = max 0 $ (c ^. availHeightL - rHeight)
        paddedImage = translateY topPaddingAmount $ result ^. imageL
        off = Location (0, topPaddingAmount)
    if topPaddingAmount == 0
      then
        return result
      else
        return $
          addResultOffset off $
            result & imageL .~ paddedImage

-- | Align bottom-right
alignBottomRightLayer :: Widget n -> Widget n
alignBottomRightLayer = vAlignBottomLayer . hAlignRightLayer
