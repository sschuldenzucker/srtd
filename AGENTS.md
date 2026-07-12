
# Documentation

See `README.md` for a high-level description of what's going on here. See its `Development Docs` section for - uh - development docs. See `docs/` for more detail.

# Version Control

This is a jj repository colocated with git. Since jj is git-compatible, you _can_ just ignore jj for read-only operations. However, using jj may give richer metadata in some cases.

- Note that we'll often be in a "detached head" state from git's perspective. This is normal.
- Use `jj log` to inspect history.
- Use `jj diff` to inspect current status.
- Feel free to use `jj new` to create checkpoints before potentially disruptive operations. Make sure every commit is named with a message starting "codex: "
- Feel free to modify history around *those* checkpoint commits starting in "codex: ".
- NEVER modify any other history unless I EXPLICITLY tell you to.
- NEVER modify immutable changes.

# Coding Style

## Overall style


## Module structure

- Use explicit module exports.
- Use haddoc sections (`-- * Section Title`) and subsections to split up long export lists and long files.
- Always add haddock comments for all exported functions and data types.
- Keep your haddock comments concise: brief description + any non-obvious behavior. Only document
  constructors and function arguments when they are not obvious.
- Use comments liberally iff they add value.
  - ALWAYS document non-obvious things: hacks, caveats, design decisions where there are multiple reasonable alternatives, very complex code, etc.
  - However, NEVER document obvious things: what every step does should be inherent in the code.
- Liberally use SOMEDAY comments for things that could be improved (made cleaner, more efficient,
  more modular, safer, etc.) in the future but where the payoff is unclear right now.

## Data Types

- Name fields with prefixes (e.g., `ac*` for fields of `AppContext`).
- Data types with a single constructor ("structs") should usually have field names specified. An
  exception are very small "utility" types with only 1-3 fields.
- Don't add unnecessary data types, in particular enums. Always check if the same need can
  be operationalized with an existing type. For example, the location where a node should be
  moved/inserted can usually be expressed using one of the `*Walker` types.
- Minimize your data types and always check which data can simply be left out. You can do this by
  checking if / how constructors are actually pattern-matched.
  - UI components shouldn't expose more than is necessary to handle the result. 

### Pattern Matching

Use the following rules for pattern matching on data types.

- When a `data` type only has a single constructor, prefer field access functions over pattern matching.
  - You can disregard this rule when a type is a small utility only used in the same module.
  - You can disregard this rule for newtypes.
- Prefer direct field views or helper functions over lens access (`foo ^. bar`) unless it improves the code.
- Prefer lens updates over restructuring.

# Known Quirks

## Brick

- adding padding around an empty widget sometimes does nothing. Restructure or use `almostEmptyWidget = str " "` instead.

