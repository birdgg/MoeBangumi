# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Backend (Haskell)
just build    # Build the project
just dev      # Run the server
just test     # Run tests
just repl     # Start REPL

# Frontend (Bun)
just web-install  # Install dependencies
just web-dev      # Start frontend dev server
just web-gen-api  # Generate API client from OpenAPI

# Combined
just dev-all  # Run backend + frontend together
```

## Architecture

### Backend (Haskell)

Three-layer architecture (DDD-style):

- `src/Moe/` - **Domain Layer**: Core entities (Bangumi, Rss, Series), parsing, business logic
- `src/App/` - **Application Layer**: Use cases, services, job scheduling
- `src/Infra/` - **Infrastructure Layer**: Database, external APIs (BGM.tv, TMDB, qBittorrent), logging
- `src/Web/` - **Web Layer**: Servant API routes and handlers

### Frontend (React)

Feature-driven architecture:

- `web/src/features/` - **Feature modules**: bangumi, calendar, downloads, settings (each with components, hooks, page)
- `web/src/components/` - **Shared components**: UI primitives (shadcn/ui), layout, theme
- `web/src/routes/` - **Routes**: TanStack Router file-based routing
- `web/src/lib/` - **Utilities**: API client (OpenAPI generated), query-client, theme

Tech stack: React 19, TanStack Query/Router/Form, Tailwind CSS, Vite

## Haskell Style

- **Comments**: Keep code comments minimal; only explain non-standard patterns
- after edit file, always apply the hlint modify
- Use `effectful` for side effects (Dynamic dispatch, TH with `makeEffect`)
- No field prefixes: use `createdAt` not `bangumiCreatedAt`
- Prefer `where` over `let`
- Always write `Display` instance for Error types using `text-display`
- Use `relude` as Prelude replacement
- use underline for field name to avoid flict with haskell functions, for example `filter_` to replace `filter`
- use `Display` instance to convert simple ADT sum type to Text
- use `inverseMap` for parse text to adt
```
data GhcVer
    = Ghc802
    | Ghc822
    | Ghc844
    | Ghc865
    | Ghc881
    deriving (Eq, Ord, Show, Enum, Bounded)

showGhcVer :: GhcVer -> Text
showGhcVer = \case
    Ghc802 -> "8.0.2"
    Ghc822 -> "8.2.2"
    Ghc844 -> "8.4.4"
    Ghc865 -> "8.6.5"
    Ghc881 -> "8.8.1"

parseGhcVer :: Text -> Maybe GhcVer
parseGhcVer = inverseMap showGhcVer
```

## Effect Conventions

When creating new Effects:
1. Define effect data type with `Dynamic` dispatch
2. Use `makeEffect` TH to generate operations
3. Write `run*` handler function
4. Add to effect stack in `Web.Types.RouteEffects` if needed for routes

## Other
- **database**, the application is not deploy yet, so all no need migration file, all in init sql file
- **setting**， when update the user preference setting, always ask me the default value