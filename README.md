
# srtd - Steffen R.'s To Do app

> [!WARNING]
> This project is **early stage**. Many features that would make it very useful are not implemented yet. It _is_ usable though and I have been happily eating my own dogfood for all my work since about a year.

srtd is a TUI (terminal) app for task management. The structure is based on arbitrarily cascading trees of items with Getting Things Done (GTD)-like statuses. Its main feature is that srtd "understands" how items relate to each other across the hierarchy. For example, a NEXT task contained in a PROJECT that is due tomorrow is itself due tomorrow and is prioritized.

srtd then queries into this hierarchy to present the tasks in a variety of useful ways (e.g., PROJECTs by urgency, WAITING items by age, NEXT actions by urgency, etc.)

I wrote srtd because I wanted an app that actually represents how I work, rather than forcing me to work around its limitations. Specifically: (1) infinite hierarchy that is actually understood by the program, (2) support for easy reprioritization and restructuring (e.g., tasks becoming projects, things getting paused and reprioritized, things being stuck waiting), (3) support for soft commitments (e.g., goal dates that are not deadlines), (4) being quick to use.

## Missing Features

This project is **early stage**. Here are a few features that aren't implemented yet but would be expected for this tool to fulfill its promise. Most of them are somewhat low-hanging fruit I think.

- **Copy & paste** of items/subtrees.
- **An inbox and processing pipeline.** (I don't really use inboxes b/c I always forget to process but maybe we can make it actually useful).
- **Notes.** I've been using NONE items for this so far and it works fine, but something explicit may be useful. This should probably just integrate plaintext files.
- **Priority markers.** I think the naive version of these is a crutch and lead to escalation (e.g., everything is prio 1 after a while; maybe we can build something actually useful here though).
- **A mobile interface.** The easiest way is to sync to todoist or something while trying to somewhat match the data structure.
- **Sync across instances.** You _can_ just put `srtd.json` on a shared drive right now if you're really careful, but no effort is made to avoid write conflicts from several running instances. So this is risky and doesn't work for teams.
- **Collaboration features.** Specifically, tracking user operations, assigning people, and appropriate filtering. (I may not implement this)
- **Better highlighting** (e.g., tinted backgrounds) to make it easier to distinguish hierarchy and statuses, and a **status bar** with various indicators (e.g., "workload" in the current view).
- **Refile** (= moving an item/subtree to a completely different location) with fuzzy find
- **Smart views** for NEXT actions that attempt to prioritize work automatically (e.g., a clever-ish weighting that includes the 4 kinds of due dates, status, age, etc., like Taskwarrior) and smart review pipelines. Careful not to over-engineer into an unused feature here!
- **Goal-specific review pipelines** (e.g., stuck projects or a project overview).
- **Better presentation** for heavily-filtered views. E.g., showing project context 
- **Binaries.** You have to build from source right now.
- **User-facing documentation** (see [Running](#running)).

## Setup

Use cabal (`cabal build`).

Tested with the following ecosystem:

- GHC 9.12.2
- cabal 3.14.2.0
- HLS 2.10.0.0
- GHCup 0.1.50.2

Previously tested with (this should still work if you're on an older GHC version):

- GHC 9.4.8
- cabal 3.10.3.0
- HLS 2.7.0.0
- GHCup 0.1.30.0

There are three packages:
- The `srtd` library
- The `srtd` executable. (run using `cabal run srtd`)
- The `srtd-test` suite. (run using `cabal run srtd-test`)

To run `safe_run.sh` (with backups), you also need [timegaps](https://gehrcke.de/timegaps/) ([repo](https://github.com/jgehrcke/timegaps)). And you my wanna use `just`.

## Running

`cabal run srtd` and Ctrl+/ to show keyboard shortcuts. Writes to `srtd.json` in the current directory. Autosaves.

See the keybindings overlay for help. Press `n` to create a new item, `s` to create a sub-items, `t` to assign statuses, and `d` to assign 4 kinds of due dates (deadline, goalline, scheduled, remind). Move items using `M-j/k`/`</>` or `M`. Use `v` to switch views/filters. Go back in submenus with `backspace` and exit submenus and overlays with `esc`. Press `C-q` from any screen to quit. Dates can be entered as ISO dates (`2025-10-29`) or in natural language (`tomorrow`/`tom`, `tue`, `in 1 week`/`1w`, etc.); time of day is supported (`13:00`). 

Use `./safe_run.sh` to pull from a dev repo, compile, and run with backups. See that script. You'll need to change config variables.

## Development Docs

srtd is written in Haskell using the [Brick](https://github.com/jtdaugherty/brick/) TUI framework. I'm using Haskell because I have _very_ limited time to work on this and debugging crashes shouldn't be part of that. This works pretty well.

Haddock docs are generated to `docs/build_haddock/` using `just docs`, and there's also more docs files there:

- [Architecture Overview](docs/architecture.md)

## Themes

A theme is a toml file. Every theme defines three toplevel keys (all of which are required):

- `theme` is a mapping from Brick's hierarchical attr names to a map of optional keys:
  - `fg`: Maps to keys in `palette`
  - `bg`: Maps to keys in `palette`
  - `style`: Maps to Brick style names. Can be a comma-separated (without whitespace) string of styles to apply multiple.
- `defaultAttr` specifies the toplevel (default) attrs, like keys in `theme`.
- `palette` is a mapping from free-form color names to color codes.
- `inherits` (optional) is the name of a theme from which `palette` and `theme` should be inherited. Note that the other required entries still *have* to be there, even if empty. The inheriting theme can overwrite individual keys within these mappings, but nothing is deep-merged. Note that `defaultAttr` is *not* inherited!

This is roughly a mix between Brick's themes (which do not support the color-name mapping layer, though) and helix's color scheme format (the only difference being that helix doesn't use a toplevel `theme` key, which I find confusing). The color schemes for of the themes are based on the respective helix scheme (`/opt/homebrew/Cellar/helix/25.01.1/libexec/runtime/themes/` on a homebrew install).

Like in helix, keys for sub-attrs must be *quoted*. I.e., to set the sub-attr `foo.bar`, you have to use this:

```toml
[theme]
"foo.bar" = {bg = "red"}
```

This is b/c there could otherwise be clashes for toplevel attrs vs the keys of sub-attrs. (SOMEDAY maybe change that; it's in principle fine as long as we don't literally call our attrs `bg` or so, which we don't. But w/e.)

## Global TODOs

- There's a memory (space) leak somewhere. I haven't figured out where yet but when you don't restart the app for a long time (days) it consumes more and more memory and becomes kinda sluggish. I think it has to do with moving items around. Restarting fixes this.

### SOMEDAY

- Maybe use [brick-panes](https://github.com/kquick/brick-panes) to abstract over the different sub-components.
  - This *might* be a better version of the `Component` class I made.
  - Check out [mywork](https://github.com/kquick/mywork), which is implemented using it.
  - Does it handle overlay or tabs? (or can it be made to?)

- Contribute some of the modules, e.g., `Aligment` to Brick.

## Comparison to other tools

There are about as many todo apps as atoms in the visible universe, here are my takes why some of them aren't enough for me:

- Todoist: Very simple data model, no understanding of hierarchies. Projects cannot have the same metadata as tasks.
- Clickup: A terrible UX, way too slow to use, and little information density. Also same issues with hierarchy as todoist.
- Taskwarrior: Poor hierarchy. I generally find it clunky to use, even with UIs.
- todo.txt: Basically no structure.
- Emacs Org Mode: I tried to use this and failed. It's supposed to be great once you get the hang of it but I didn't.
- [Smos](https://github.com/NorfairKing/smos?tab=readme-ov-file): Probably closest to srtd. It's very opinionated (as is srtd) but some of the opinions never quite clicked for me.


