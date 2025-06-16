
# Architecture

Here's some overview of the architecture in srtd.

# Data Model

## Data Structures

The tree (really a forest) of items is stored in memory as what's currently just a `Data.Tree` where the label data consists of an ID together with attached attribute data (e.g., title, deadline, etc.,) + derived data. Every item (= node in the tree) has a unique ID. IDs (of type `EID`) are UUIDs for normal items plus special IDs for the toplevel items `VAULT` etc. (only `VAULT` being currently used). We have the [`IdTree`](../docs/build_haddock/srtd/Srtd-Data-IdTree.html) wrapper structure to abstract over this fact a little bit (but very imperfectly).

A cascading series of attribute structures is used for each node:

- `Attr` = "physical" node attributes that are stored in the data file. (e.g. title, deadline) 
- `DerivedAttr` = globally derived node attributes that are stored in the global in-memory model and dynamically computed (e.g., bottom-up actionability, implied deadline of children). Derived from the above.
- `LocalDerivedAttr` = locally derived node attributes that depend on the view in question. These are stored in per view (`MainTree` instances). Derived from the above and view metadata (focused root node, applied filters for collapse/expand metadata)

## Data Storage

The tree of `(ID, Attr)` values is stored in/loaded from a plain JSON file. We use a separate `ModelJSON` data structure for this to simplify the implementation.

## Actors

We use a few actors to coordinate access to the model. This is currently not used much but should make it easy in the future to interact with other systems (e.g., sync, notifications, etc.).

A central `ModelServer` actor governs the in-memory data model and has a simple pubsub mechanism to let other actors be notified of changes to the model. This generates `ModelUpdated` messages to the app and notifies the `ModelSaver` of changes (see right below).

NOTE: Currently, model updates _or_ notifications aren't really async: the calling thread is blocked until the model is updated and notifications are sent, and the `ModelServer` only regulates access to the model. Model updates are lazy, so nothing really happens on update (this might be good or bad). Note that this means that most model updates currently _look_ async (and could be) but are really not: the app thread modifies the model, sends update notification to the app channel (and others), that brick update finishes and then, Brick processes the `ModelUpdated` event. In practice, the update itself is likely evaluated by the `ModelSaver` since it'll probably the first to see it. 

A separate `ModelSaver` actor subscribes to model updates and overwrites the data file on each change (i.e., we have autosave). This is actually a separate thread and also does some throttling b/c writing JSON is actually kinda slow, and has some safety mechanisms so we don't corrupt our file.

## SOMEDAY some potential improvements

- `Data.Tree` is great for running hierarchical algorithms but _terrible_ for (1) looking stuff up by ID and (2) making small modifications, both of which are very common operations. We may want to replace `IdTree` by an actually different data structure that tracks nodes using IDs. I had a working version of that using 6 or so maps of form `ID -> ID` in Elixir, could migrate that. Careful testing would be required to see if it's actually faster though. I didn't have any issues so far (except for a weird space leak), though that may also just be b/c I have a big machine.
  - We may wanna make this a concurrent data structure so that we don't actually need an actor for the model (which can become a bottleneck) and updates to distinct parts of the tree can be concurrent. And/or we may want an actor but use TRef's to store data as updatable pointers.
  - Then again all of this may well be pointless b/c our volume is quite low.
- Recomputation of derived attrs is trivial right now: each change leads to a full recompute. That wouldn't be necessary in principle.
- `EID` is not really correct since the special ones can only occur at the toplevel.
- There's no undo/redo right now. We might want to migrate to a diff/change-event structure with regular snapshots instead for the in-memory model. This wouldn't affect `Model` (I think) but would make `Model` itself basically a cache.
- That JSON file is becoming quite large (e.g., my prod file is ~3MB by now). We might wanna compress or use some more efficient format. Hasn't been a problem though.
- We may wanna make the `ModelServer` actually async at some point. And give it more introspectable data than just a function to update the model. This may be required for undo/redo or to connect to other systems (sync, for instance).
- `ModelServer` currently loads a (1) hard-coded data file (2) on its startup, which is obv a bit silly.

# UI

## UI Components

We use Brick as the fundamental UI framework but add our own custom superstructure to deal with the following things not natively handled by brick:

1. Often we want to quasi-"call" a UI component to get some input from the user (e.g., a text for a title, a date for a deadline etc.). We then route key events to the component until the component tells us it's done and provides the resulting value. This structure needs to be recursive.

2. Components shouldn't do _all_ their rendering themselves but some details should be decided by the parent because it has more context. These include: (1) where to render, (2) the format (e.g. overlay window vs split vs main window), (3) if/where to show keybindings.

3. Some context needs to routed from the app layer (e.g., the current time, where to reach different services).

We handle this using the `AppComponent` class in [`Srtd.Component`](../docs/build_haddock/srtd/Srtd-Component.html). Most UI code is built on top of this, see `Srtd.Components.*`.

Note: This setup is a bit ad-hoc and may be similar to [brick-panes](https://github.com/kquick/brick-panes). SOMEDAY we may wanna use that instead; see the readme.

## UI (A)Synchronicity

Keyboard events and custom app events (see [`Srtd.Component`](../docs/build_haddock/srtd/Srtd-Component.html)) are handled synchronously and sequentially through Brick's system (using the `EventM` monad). This includes `ModelUpdated` events, where components receve an (at that point async) message from the `ModelServer` but then synchronousyly need to pull the actual new model.

Instructions to _change_ the model are usually sent asynchronously from components (specifically `MainTree` and associated child components) to the `ModelServer` and then result in a `ModelUpdated` event. Some actions need to do this synchronously (e.g. adding an item). 

We have a special `Tick` app event that makes components update their internal state (but not pull a new model) once per minute to see, e.g., if a reminder time has passed. We don't do notifications right now, this is only for display.

When a filter is updated (filter changed, also on collapse/expand), a new model is pulled and the whole filter is run.

SOMEDAY there are some suboptimal aspects of this approach that could be improved in the future:

- The `ModelUpdated` event could just include the actual new model, which is free (it's just a pointer). Saves a pull.
- There's no reason why updates on independent components need to be sequential. They could be parallel, but the `EventM` monad is in the way here. We could just get rid of this / only use it at the application level, and use whatever other / our own structure. Maybe make each component an actor or something, or optionally so.
- Sync-updating the model is ugly just to get the new EID of an added item. Would be great if we could attach metadata to the update (specifically, the ID (?) of the view that updated it and/or a continuation).
- More generally, there are _many_ places in the UI that would benefit from reactive programming techniques.
- It may not be the best thing in the world that MainTree is stateless about items. E.g. in a more normal approach, we would probably just attach a "collapsed" flag to nodes. Our approach is very clean (e.g., for async updates by others it's clear what to do) but also a bit funky and prob slow.

## UI error handling

There are (unfortunately) at several ways of handling errors. The most common type of "real" error is that you want to do something with an item that has been deleted (either directly or through its parent). Handling mehtods:

- With in the `MainTree` component, the `EventMOrNotFound` monad extension in `MainTree` uses a monad failure for the specific case of unknown item IDs.
- At the component level, usually error simply leads to a `Canceled` return value, i.e., the component is closed without action. Use `notFoundToAER_` to convert between the former and this one.
- "Errors" that are really just interactions not currently available (like going up when already at the top) lead to no action with a `Continue` return value.

Model-level operations generally don't have possible failure attached. In the (considered rare) case where they do fail (e.g., a true race condition where an item is deleted and also we try to operate on that item) they simply do nothing.

SOMEDAY there may be some of these race conditions hidden by the fact that the UI doesn't allow too much to be done async right now. For example, what happens when I open the "set deadline" dialog, then the item gets deleted, then I confirm?

## Rendering and theming

Rendering is done using brick, and we use Brick's "attr" system to assign attributes to UI items (color etc.). For theming, we assign attrs using Brick's `attrMap` mechanism and we _do_ use Brick themes to configure them, but we use our own custom theming mechanism to configure those themes using toml files; this is mostly taken from the helix editor. See the readme.

SOMEDAY The way how Brick's hierarchical attrs cascade is actually terrible; it's _not_ like CSS! That's why we need to write, e.g., `selected.status.next` instead and explicitly instead of the more intuitive `status.next.selected` (and expecting to get the same as `status.next` if the `.selected` variant is not specified). 
