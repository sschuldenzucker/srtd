
# Srtd - A task management app for humans

## Setup

Use cabal (`cabal build`).

Tested with the following ecosystem:

- GHC 9.4.8
- cabal 3.10.3.0
- HLS 2.7.0.0
- GHCup 0.1.30.0

There are three packages:
- The `srtd` library
- The `srtd` executable. (run using `cabal run srtd`)
- The `srtd-test` suite. (run using `cabal run srtd-test`)

To run `safe_run.sh` (with backups), you also need [timegaps](https://gehrcke.de/timegaps/) ([repo](https://github.com/jgehrcke/timegaps)).

## Running

`cabal run srtd` and Ctrl+/ to show keyboard shortcuts. Writes to `srtd.json` in the current directory. Autosaves.

## Themes

A theme is a toml file. Every theme defines three toplevel keys (all of which are required):

- `theme` is a mapping from Brick's hierarchical attr names to a map of optional keys:
  - `fg`: Maps to keys in `palette`
  - `bg`: Maps to keys in `palette`
  - `style`: Maps to Brick style names. Can be a comma-separated (without whitespace) string of styles to apply multiple.
- `defaultAttr` specifies the toplevel (default) attrs, like keys in `theme`.
- `palette` is a mapping from free-form color names to color codes.
- `inherits` (optional) is the name of a theme from which `palette` and `theme` should be inherited. Note that the other required entries still *have* to be there, even if empty. The inheriting theme can overwrite individual keys within these mappings, but nothing is deep-merged. Note that `defaultAttr` is *not* inherited!

This is roughly a mix between Brick's themes (which do not support the color-name mapping layer, though) and helix's color scheme format (the only difference being that helix doesn't use a toplevel `theme` key, which I find confusing).

Like in helix, keys for sub-attrs must be *quoted*. I.e., to set the sub-attr `foo.bar`, you have to use this:

```toml
[theme]
"foo.bar" = {bg = "red"}
```

This is b/c there could otherwise be clashes for toplevel attrs vs the keys of sub-attrs. (SOMEDAY maybe change that; it's in principle fine as long as we don't literally call our attrs `bg` or so, which we don't. But w/e.)

## Global TODOs

### SOMEDAY

- Maybe use [brick-panes](https://github.com/kquick/brick-panes) to abstract over the different sub-components.
  - This *might* be a better version of the `Component` class I made.
  - Check out [mywork](https://github.com/kquick/mywork), which is implemented using it.
  - Does it handle overlay or tabs? (or can it be made to?)

- Contribute some of the modules, e.g., `Aligment` to Brick.

