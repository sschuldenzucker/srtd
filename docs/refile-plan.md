# Refile Plan

Refile moves the selected subtree to a destination parent chosen through a searchable tree picker. It is a general `MainTree` feature, with inbox processing as the motivating use case: an inbox item is processed when it leaves direct-child-of-`INBOX`.

Status: first slice implemented.

## Critical Review

The proposed shape is good and fits srtd:

- Refile should be general, not inbox-specific. The model already treats items as movable subtrees, and inbox processing is just one case of organizing.
- `SPC R` for global refile is the right primary operation. Processing inbox items needs access to the real planning tree, not just the current inbox root.
- `SPC r` for local refile is plausible as a secondary organizing tool. It is useful for reshaping a project without leaving that project's context.
- Reusing `QuickFilter` is the right implementation direction. It already provides a searchable tree view and has variant infrastructure for different confirmation behavior.
- Regex matching is fine for the first slice. Fuzzy/hierarchical matching should be a broader search redesign affecting quick jump, search, and refile together.

Main design caveats:

- Refile needs a clear default placement. For the first slice, selecting a destination should mean "move selected subtree as last child of destination." This is ideal for inbox processing and avoids a second placement prompt.
- The destination root itself must be selectable somehow. Global refile often wants "move this inbox item to top-level under `VAULT`"; local refile often wants "move this item directly under the current root." Current QuickFilter-style tree views may only expose children, not the root header.
- The current item and its descendants must not be valid destinations. Moving a node below itself is nonsensical and currently lower-level move code may silently no-op.
- Global refile should search below `VAULT`, not all top-level roots. Refiling into `INBOX` or `CLIPBOARD` is not a meaningful primary workflow.
- Local refile from `INBOX` is not the inbox-processing feature. It may still work, but `SPC R` is the important path for processing.

## User-Facing Design

### Global Refile

`SPC R`: refile the selected subtree somewhere under `VAULT`.

Flow:

1. User selects an item or subtree anywhere.
2. User presses `SPC R`.
3. srtd opens a QuickFilter-based destination picker rooted at `VAULT`.
4. User filters with the current regex search behavior.
5. User selects a destination parent.
6. User confirms with one of the refile placement keys.
7. The picker closes.

For inbox processing, the user can:

1. `g I`
2. Select first inbox item.
3. `SPC R`
4. Pick destination under `VAULT`.
5. Confirm.

The item is now processed because it is no longer a direct child of `INBOX`.

### Local Refile

`SPC r`: refile the selected subtree within the current root.

Flow is the same as global refile, except the destination picker is rooted at the current `MainTree` root instead of `VAULT`.

This is mainly for project cleanup and local restructuring.

## Root-as-Destination

First-slice refile needs a way to choose the destination root itself:

- In global refile, target `VAULT`.
- In local refile, target the current root.

Decision for the first implementation:

- Use a documented special key, probably `M-Enter`, to choose the picker root as destination.
- Keep regular `Enter` as "choose selected visible destination."

I also prefer showing the root as a first target row in principle. After checking the current code, that is not the small path for the first slice: `TreeView` renders `stForest` children as the selectable list, while the root is stored separately as `rootLabel` and does not have a `LocalDerivedAttr`. Making the root a real selectable row would likely require a TreeView option to synthesize a root list item, adjust child levels, update filtering/reload behavior, and keep quick jump behavior unchanged.

This is worth doing later if root selection becomes common enough. For now, `M-Enter` is acceptable because top-level filing below `VAULT` is less common than filing below long-lived realms/projects.

## Target Filtering

Destination picker should exclude:

- The selected source item.
- All descendants of the selected source item.
- `INBOX` and `CLIPBOARD` for global refile, by construction, because global refile is rooted at `VAULT`.

For local refile, if the selected source is the current root, refile should no-op or be unavailable. Usually the selected item is below the current root, so this is not the common case.

## Placement

First slice:

- `Enter`: move as last child of the selected destination. This is the default and should stay the muscle-memory path for inbox processing.
- `M-f`: move as first child of the selected destination.
- `M-j`: move after the selected destination, as its next sibling.
- `M-k`: move before the selected destination, as its previous sibling.
- `M-Enter`: move as last child of the picker root (`VAULT` for global refile, current root for local refile).
- `M-F`: move as first child of the picker root.

Sibling placement for the picker root is not meaningful, so there is no root variant for before/after.

Why modified keys:

- Plain character keys need to keep editing the regex query.
- `M-j`/`M-k` already mean movement-like operations elsewhere in srtd, so using them for after/before is reasonably mnemonic.
- `M-f` is a little arbitrary, but "first child" is the natural non-default child placement.
- A submenu would be more discoverable but slower for the 80% last-child case and awkward inside a text-entry overlay.

Later:

- Revisit if `M-f` feels awkward in practice.
- Add a visible placement hint in the overlay key help.
- Consider a placement mini-menu only if the modifier keys are too hard to remember.

For inbox processing, "last child" is the right default because the user is usually filing into an existing project or area.

## Likely Code Touch Points

- `Srtd.Components.MainTree`
  - Done: add `SPC R` and `SPC r` to `spaceKeymap`.
  - Done: add `pushRefileOverlay`.
  - Done: on confirm, call a model move operation that moves the selected subtree relative to the selected/root destination with the requested insert walker.
- `Srtd.Components.QuickFilter`
  - Done: keep existing `NodeSelection` unchanged for quick jump.
  - Done: add `RefileDestinationSelection`.
  - Done: return both destination and placement from the refile picker.
  - Done: keep regex filtering for now.
- `Srtd.Model`
  - Done: add `moveSubtreeRelToAnchor :: EID -> EID -> InsertWalker IdLabel -> Model -> Model`.
  - Done: wrap `forestMoveSubtreeIdRelToAnchorId source destination insertWalker`.
  - Done: add tests for child placements, sibling placements, and no-op behavior for invalid self/descendant destinations.

## Verification

Manual smoke tests:

1. From `INBOX`, select an item and `SPC R`; move it below a `VAULT` destination.
2. Confirm the inbox count decreases.
3. Confirm the item appears as last child of the destination.
4. Confirm `M-f` moves it as first child of the selected destination.
5. Confirm `M-j` and `M-k` move it after/before the selected destination.
6. Confirm the current item cannot be refiled into itself or its descendants.
7. From a project root, use `SPC r` to move a child under another local destination.
8. Confirm global refile does not offer `INBOX` or `CLIPBOARD`.
9. Confirm `M-Enter` and `M-F` refile to the picker root for both global and local refile.
10. Confirm quick jump still behaves exactly as before.

Automated checks:

- Done: add model tests for the pure move helper.
- Done: run `cabal build`.
- Done: run `cabal test`.

## Later Improvements

- Fuzzy matching and better search modes shared by quick jump, search, and refile.
- Show the picker root as an explicit first selectable row if TreeView grows clean support for root rows.
- More explicit placement choices if first-slice modifier keys are not enough.
- Option to auto-follow the item across refile. This is not the default and is somewhat unclear because the new root is not always obvious.
- Optional success flash/status message after refile.
- A richer inbox processing view only if refile plus normal `MainTree` editing still feels too loose.
- An easy free feature is then global quick jump (`SPC J`). This project should put all the pieces there.

## Implementation Sketch

Likely shape:

```haskell
data NodeSelectionOrRoot = NodeSelectionOrRoot EID

data NodeOrRootSelected
  = NodeDestinationSelected RefilePlacement (Maybe (CompiledWithSource Regex)) EID
  | RootDestinationSelected RefilePlacement EID

data RefilePlacement
  = RefileLastChild
  | RefileFirstChild
  | RefileAfter
  | RefileBefore
```

The `NodeSelectionOrRoot` variant would behave like `NodeSelection` for regular `Enter`, and add modified-key confirms:

```haskell
extraKeys =
  [ kmLeaf (binding KEnter []) "Last child" $
      confirmSelected RefileLastChild
  , kmLeaf (binding (KChar 'f') [MMeta]) "First child" $
      confirmSelected RefileFirstChild
  , kmLeaf (binding (KChar 'j') [MMeta]) "After" $
      confirmSelected RefileAfter
  , kmLeaf (binding (KChar 'k') [MMeta]) "Before" $
      confirmSelected RefileBefore
  , kmLeaf (binding KEnter [MMeta]) "Root last child" $
      return $ Confirmed (RootDestinationSelected RefileLastChild rootEID)
  , kmLeaf (binding (KChar 'F') [MMeta]) "Root first child" $
      return $ Confirmed (RootDestinationSelected RefileFirstChild rootEID)
  ]
```

Implemented with this general shape. `MainTree` uses the refile-specific variant for both global and local refile without altering quick jump's existing `NodeSelection` use.

## Decisions

- Use `M-Enter` for root-as-destination in the first slice.
- Include first-child and sibling placement in the first slice; default `Enter` remains last child.
- Keep global refile rooted at `VAULT`; do not offer `INBOX` or `CLIPBOARD`.
- Keep local refile available everywhere, including `INBOX` and `CLIPBOARD`.
- Do not follow the moved item by default. When refiling from `INBOX`, keep the root as `INBOX` and let normal no-follow behavior land on the next inbox item if available.
