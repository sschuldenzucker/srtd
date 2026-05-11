# Quick Add Inbox Plan

This note sketches a deliberately small quick-add flow:

1. Press a shortcut from the main tree.
2. Enter one item title in a dialog.
3. Confirm to append that item to `INBOX`.
4. Process the inbox later with existing navigation and cut/paste commands.

The goal is speed and low implementation risk. This should make capture cheap without trying to solve inbox processing yet.

## First Slice

Status: done.

Add a root-level `a` binding in the main tree keymap:

- Opens a one-line text overlay titled something like `Quick Add to INBOX`.
- Uses the existing `NewNodeOverlay` unless we discover it needs small generalization.
- On `Enter`, creates a new normal item with the entered title.
- Appends the item as the last child of the top-level `INBOX` node.
- Leaves the current view/root/focus alone after insertion.
- On `Esc`, cancels without changing the model.

Also add `g I` to navigate to `INBOX`, matching the existing `g C` for `CLIPBOARD` and `g V` for `VAULT`.

This should work from any current root, including `VAULT`, `INBOX`, `CLIPBOARD`, or a hoisted subtree.

## Data Behavior

The new item should be the same kind of item created by the existing `n` flow:

- `EIDNormal` generated from `nextRandom`.
- `Attr` from `attrMinimal` using the current time.
- Default status remains whatever `attrMinimal` currently chooses.
- No dates or other metadata are set.
- Insert position is `insLastChild` below `Inbox`.

Empty input should keep the current `NewNodeOverlay` behavior, which means confirming an empty overlay creates an empty item. We can make that stricter later.

## Likely Code Touch Points

- `Srtd.Components.MainTree`
  - Add the `a` binding to `rootKeymap`.
  - Add the `g I` binding to `goKeymap`.
  - Add a small helper like `pushQuickAddToInbox`.
  - Reuse `pushOverlay`, `newNodeOverlay`, `attrMinimal`, `nextRandom`, and `insertNewNormalWithNewId`.
- `Srtd.Model`
  - Probably no model change needed for the first slice.
  - Optional cleanup later: add a named helper such as `appendNewNormalToInboxWithNewId` if the UI helper feels too model-aware.
- Tests
  - Add a pure model test if we introduce a model helper.
  - Otherwise, this may initially be covered by build/typecheck plus manual TUI verification.

## Interaction Details

Suggested keybinding:

- `a`: quick-add one item to inbox.
- `g I`: go to inbox.

Suggested overlay behavior:

- `Enter`: confirm.
- `Esc`: cancel.
- All normal text editing remains delegated to the existing editor component.

The first slice should not automatically switch to `INBOX`, focus the newly-created item, or show a confirmation after adding. Capture should feel like "drop this thought into the tray and continue where I was."

## Inbox Processing

For now, processing is just:

- Navigate to `INBOX` with `g I`.
- Use existing cut/paste commands to move items into the right place.
- Edit status/dates after moving, using current commands.

## Verification

Manual smoke test:

1. Start `srtd`.
2. Press `a`.
3. Type a title and press `Enter`.
4. Confirm the current view did not jump.
5. Go to `INBOX`.
6. Confirm the new item is appended after existing inbox items.
7. Press `a`, then `Esc`; confirm no item is added.
8. Try an empty confirm; confirm an empty item is added, matching the current `NewNodeOverlay` behavior.

Automated checks:

- Run `cabal build`.
- Run `cabal test` or `cabal run srtd-test`.

## Later Improvements

- [x] Add a status bar hint for the size of the inbox. This sits in the tab bar right-side indicator next to the clipboard count.
- Somehow deal with the fact that the user often doesn't just want to add single items, but whole subprojects / trees.
  - One easy way would be to add an alt-confirm mode to the quick-add overlay that opens INBOX in a new tab and focuses the just-added item.
  - This would also be a lightweight way to deal with setting dates and statuses etc.
- Make the NewNodeOverlay richer and/or parse lightweight syntax for status/dates, e.g. `call Alex @tomorrow #waiting`.
  - Syntax may be a pain or maybe ok. Needs some deliberation if it should be this or just additional fields with keyboard shortcuts.
  - This also applies to general node adding and editing. Could be the same interface.
- Add an inbox processing workflow with next/skip/refile commands.
  - OPEN: Should this be an entirely new workflow or should it be new features added to the regular MainTree interface?
  - In other words, should the PROCESS and ORGANIZE processes in GTD have the same technical implementation?
  - Intuition tells me no but carefully explore if there is _actual_ value from having a separate PROCESS interface. There may not be.
- Add quick refile/fuzzy move from inbox.
  - Again, I think we want this from everywhere. Probably, unless PROCESS is a separate workflow.

## Decisions

- Keep `g I` as the inbox navigation binding.
- Keep quick-add scoped to the main tree.
