
# Documentation

See `README.md` for a high-level description of what's going on here. See its `Development Docs` section for - uh - development docs. See `docs/` for more detail.

# Coding Style

## Overall style

- Prefer simple constructs.
- Prefer direct field views or helper functions over lenses `foo ^. bar` unless it improves the code.

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

### Pattern Matching

Use the following rules for pattern matching on data types.

- When a `data` type only has a single constructor, prefer field access functions over pattern matching.
  - You can disregard this rule when a type is a small utility only used in the same module.
  - You can disregard this rule for newtypes.

# Known Quirks

## Brick

- adding padding around an empty widget sometimes does nothing. Restructure or use `almostEmptyWidget = str " "` instead.

