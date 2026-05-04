
# Documentation

See `README.md` for a high-level description of what's going on here. See its `Development Docs` section for - uh - development docs. See `docs/` for more detail.

# Coding Style

## Overall style

- Prefer simple constructs.
- Prefer direct field views or helper functions over lenses `foo ^. bar` unless it improves the code.

## Module structure

- Use explicit module exports.
- Always add haddock comments for all exported functions and data types.
- Keep your haddock comments concise: brief description + any non-obvious behavior. Only document
  constructors and function arguments when they are not obvious.
- Use comments liberally but only for non-obvious things: hacks, caveats, design decisions where
  there are multiple reasonable alternatives, etc.
- Liberally use SOMEDAY comments for things that could be improved (made cleaner, more efficient,
  more modular, safer, etc.) in the future but where the payoff is unclear right now.

