-- | Shared rendering for tree status bars and breadcrumb-like tree context.
module Srtd.Components.TreeStatusBar (
  BreadcrumbDirection (..),
  renderBreadcrumbs,
  renderLabelShort,
  renderStatusActionabilityCounts,
  statusBarW,
) where

import Brick
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Time (ZonedTime)
import Srtd.AppAttr qualified as AppAttr
import Srtd.Attr
import Srtd.Components.Attr (
  actionabilityAttr,
  mostUrgentDateAttr,
  renderMostUrgentDateMaybe,
  renderStatus,
 )
import Srtd.Data.MapLike qualified as MapLike

-- | Direction to use when displaying an item's ancestor context.
data BreadcrumbDirection
  = BreadcrumbsLeafFirst
  -- ^ Render context from the selected item toward the root.
  | BreadcrumbsRootFirst
  -- ^ Render context from the root toward the selected item.

-- | Render the selected item's ancestor path and descendant status counts.
statusBarW :: BreadcrumbDirection -> ZonedTime -> Maybe LocalIdLabel -> Widget n
statusBarW breadcrumbDirection now mcur =
  withDefAttr AppAttr.header_row $
    padRight Max selectedBreadcrumbsW <+> statusBarRightW
 where
  selectedBreadcrumbsW = case mcur of
    Nothing -> emptyWidget
    Just illabel ->
      overrideAttr AppAttr.breadcrumbs AppAttr.header_row $
        renderBreadcrumbs breadcrumbDirection now (map localIdLabel2IdLabel . gLocalBreadcrumbs $ illabel)
  statusBarRightW =
    hBox
      [ maybe emptyWidget (renderStatusActionabilityCounts . daNDescendantsByActionability . getDerivedAttr) mcur
      , -- Spacer b/c I find it hard to read stuff at the right side of the screen
        str " "
      ]

-- | Render a path of ancestor labels in the requested direction.
--
-- NB we don't shade colors in the breadcrumbs so they "pop out", but I consider this a feature
-- rather than a bug for now.
renderBreadcrumbs :: BreadcrumbDirection -> ZonedTime -> [IdLabel] -> Widget n
renderBreadcrumbs direction ztime breadcrumbs =
  withAttr AppAttr.breadcrumbs $
    hBox $ case direction of
      BreadcrumbsLeafFirst ->
        [x | (_, lbl) <- breadcrumbs, x <- [str " < ", renderLabelShort ztime lbl]]
      BreadcrumbsRootFirst ->
        [x | (_, lbl) <- reverse breadcrumbs, x <- [renderLabelShort ztime lbl, str " > "]]

-- | Render a short (one-line version of the item) with dates but without status
renderLabelShort :: ZonedTime -> Label -> Widget n
renderLabelShort ztime (attr, dattr) =
  hBox
    [ str (name attr)
    , maybe
        emptyWidget
        wrapDateWidget
        (renderMostUrgentDateMaybe ztime False (dates attr) (daImpliedDates dattr))
    ]
 where
  wrapDateWidget w =
    -- SOMEDAY it's kinda stupid that this does the same calculation again as above
    let battr = mostUrgentDateAttr ztime False (dates attr) (daImpliedDates dattr)
     in hBox [str " ", withAttr battr (str "["), w, withAttr battr (str "]")]

-- | Render compact counts of descendant statuses and project actionability.
renderStatusActionabilityCounts :: StatusActionabilityCounts -> Widget n
renderStatusActionabilityCounts sac =
  -- SOMEDAY There _may_ be a performance issue b/c we vary the created set of widget on selection
  -- move.
  -- SOMEDAY show Done/Canceled statuses?
  hBox . concat $
    [ intersperse sepIntraGroup
        . catMaybes
        $ [ maybeRenderIndicatorSingle WIP
          , maybeRenderIndicatorSingle Next
          , maybeRenderIndicatorSingle Waiting
          -- , maybeRenderIndicatorSingle Later
          -- , maybeRenderIndicatorSingle Open
          ]
    , [sepSingleProjects]
    , intersperse sepIntraGroup . catMaybes $
        map maybeRenderIndicatorProject displayedProjectActionabilities
          ++ [maybeStuckProjectsActionabilitiesW]
    ]
 where
  -- Not rendering Opens by actionability b/c I think that's too much information
  -- SOMEDAY alt, if n == 0, show the number and the indicator but in standard gray. Could make it less busy.
  sepMarkerCount = emptyWidget
  sepSingleProjects = str " | "
  sepIntraGroup = str " "
  maybeRenderIndicatorSingle s =
    let n = MapLike.findWithDefault 0 s . sacSingleStatuses $ sac
     in if n == 0
          then Nothing
          else
            Just $
              withAttr (actionabilityAttr False s) $
                hBox [renderStatus False s s, sepMarkerCount, str . show $ n]
  maybeRenderIndicatorProject a =
    let n = MapLike.findWithDefault 0 a . sacProjects $ sac
     in if n == 0
          then Nothing
          else
            Just $
              withAttr (actionabilityAttr False a) $
                hBox [renderStatus False Project a, sepMarkerCount, str . show $ n]
  -- Not displaying LATER b/c I think that's basically stuck and we need to prune down items.
  displayedProjectActionabilities = [WIP, Next, Waiting]
  maybeStuckProjectsActionabilitiesW =
    let n = sacNStalledProjects sac
     in if n == 0
          then Nothing
          else
            Just $
              withAttr (actionabilityAttr False Someday) $
                hBox [renderStatus False Project Someday, sepMarkerCount, str . show $ n]
