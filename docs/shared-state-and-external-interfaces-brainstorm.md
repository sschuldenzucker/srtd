# Shared State and External Interfaces Brainstorm

This note explores how `srtd` could evolve from "one TUI owns one local JSON file" into "multiple clients interact with one task state." The motivating use cases are quick-add from outside the TUI and an MCP server for AI agents, but the design should leave room for other clients without turning the core app into protocol soup.

My strong opinion: the next architectural boundary should not be "add a web server." It should be "make normal client model changes explicit, structured, and serializable." Once `srtd` has a command layer, the TUI, CLI, MCP server, sync process, and future clients can all become frontends over the same core behavior.

That does not mean abandoning pure model endomorphisms. The better split is:

- Normal clients submit structured commands.
- Haskell clients import the command type library and use client functions that submit those commands.
- Same-process clients can submit commands without real encoding.
- Other-process clients encode the same command values into a wire format and send them to an exposed endpoint.
- The model server compiles validated commands into internal `Model -> Model` transformations and executes those transformations atomically.

So commands are the public contract; endomorphisms remain the pure execution engine. This keeps the hard parts testable and Haskell-native without making opaque functions the sync or API protocol.

## Useful Use Cases

### Capture From Anywhere

- Shell quick-add: `srtd add "Email Alex" --status next --scheduled tomorrow`.
- Launcher quick-add: Raycast/Alfred/Spotlight-style prompt that sends one line to `INBOX`.
- Editor quick-add: from Vim/Helix/VS Code, capture selected text as a task or note.
- Git-aware capture: create a task from the current repo/branch/file/line.
- Browser/email capture: send a URL, title, selected text, and source metadata.
- Voice/mobile capture: low-fidelity "dump this into the inbox" from a phone or assistant.
- Clipboard capture: turn clipboard contents into an inbox item or child notes.

### AI-Agent Collaboration

- List actionable tasks matching a context, date range, project, or query.
- Add tasks, notes, child tasks, waiting items, and reminders during a work session.
- Refile or propose refile destinations for inbox items.
- Summarize a project subtree and identify stale or blocked parts.
- Ask "what should I work on next?" using the same query semantics as the TUI.
- Attach provenance: "created by Codex thread X", "suggested by agent", "confirmed by user."
- Stage changes as proposals instead of directly mutating the task tree.

### Sync and Multi-Client Workflows

- Run the TUI and a quick-add command at the same time without clobbering JSON.
- Run multiple TUI instances on the same machine.
- Keep a read-only dashboard open while editing in the TUI.
- Sync desktop and laptop state with conflict handling.
- Export a narrowed view to another task tool while keeping `srtd` canonical.
- Import from another system into `INBOX` or a review queue.
- Allow one trusted background process to own reminders and notifications.

### Automation and Review

- Notification daemon: watch reminders/scheduled dates and trigger system notifications.
- Daily/weekly review generator: create a review list from stale projects and waiting items.
- Calendar bridge: surface deadline/goalline/scheduled/remind dates externally.
- Recurring task generator: insert recurring tasks as explicit future items.
- Metrics: count waiting age, inbox growth, project stuckness, review load.
- Backups, snapshots, and timeline inspection.

### Sharing and Collaboration

- Share one project subtree with another person or tool.
- Assign ownership without making collaboration infect every local-only task.
- Comment or activity stream on task changes.
- Redaction/export profiles so AI/tools do not see all private tasks.
- Capability-scoped clients: capture-only, read-only, project-only, full-admin.

## Conceptual Refactor

### Current Shape

The current model is:

- `Srtd.Model` contains the in-memory forest and pure model operations.
- `Srtd.ModelServer` stores one `TVar Model`, exposes `getModel`, and accepts opaque functions `Model -> Model`.
- `Srtd.ModelSaver` subscribes to update notifications and rewrites `srtd.json`.
- The TUI is currently the main client and often assumes updates are synchronous.

This is a good local architecture for one app. The problem is not that endomorphisms are bad; the problem is that opaque Haskell functions are not a good normal-client protocol. They cannot be logged, serialized, authorized, replayed, merged, explained, or exposed through MCP in a stable way.

### Desired Shape

Think in layers:

1. Domain model: pure data types and pure operations.
2. Command API: explicit normal-client intents such as `CreateNode`, `MoveSubtree`, `SetStatus`.
3. Command compiler/interpreter: validates commands and turns them into pure model endomorphisms plus metadata.
4. Event log or change API: facts that happened such as `NodeCreated`, `SubtreeMoved`.
5. Repository/store: durable state, snapshots, transactions, and conflict detection.
6. Service/runtime: one process that serializes writes and publishes updates.
7. Clients: TUI, CLI, MCP, notification daemon, import/export tools.

The key split is between commands and events:

- Commands are requests. They can fail, require permissions, allocate IDs, parse dates, and depend on current state.
- Events are accepted changes. They should be replayable and stable enough to support history, undo, sync, and debugging.

For a first slice, commands alone may be enough. The long-term architecture should still keep event logging in mind, but undo/sync/event-sourcing can be evaluated separately from the immediate "normal clients submit structured commands" refactor.

## Concrete Code Direction

### 1. Introduce a Structured Command Type

Add a module such as `Srtd.Model.Command`:

```haskell
data ModelCommand
  = CreateNode CreateNodeCommand
  | MoveSubtree MoveSubtreeCommand
  | UpdateAttr UpdateAttrCommand
  | DeleteSubtree DeleteSubtreeCommand
  | CopyToClipboard CopyToClipboardCommand
  | PasteFromClipboard PasteFromClipboardCommand
```

Each command should have enough data to be serializable. Do not store callbacks, lenses, walkers-as-functions, or arbitrary `Model -> Model` mutations in this type.

Some current concepts like `InsertWalker` may need a serializable sibling:

```haskell
data InsertPosition
  = AsFirstChildOf EID
  | AsLastChildOf EID
  | Before EID
  | After EID
```

The pure interpreter can translate `InsertPosition` into existing walker helpers internally.

This command type should be part of a small client-facing library. Haskell clients should not have to hand-roll JSON. They should construct typed commands and pass them to a submit function. Depending on runtime topology, that submit function can be a no-op in-process call, JSON-RPC, REST, a Unix socket protocol, MCP adapter glue, or something else.

### 2. Make Command Execution Return a Result

Add an execution boundary that compiles commands into internal model transformations:

```haskell
compileModelCommand ::
  (?mue :: ModelUpdateEnv) =>
  ModelCommand ->
  Model ->
  Either ModelCommandError CompiledModelCommand

data CompiledModelCommand = CompiledModelCommand
  { cmcApply :: (?mue :: ModelUpdateEnv) => Model -> Model
  , cmcResult :: ModelCommandResult
  }
```

`ModelCommandResult` should include:

- IDs created or affected.
- Optional focus hints for TUI clients.
- A concise description for logs/debug views.
- Eventually, produced events.

The server would apply `cmcApply` atomically and return `cmcResult` after the new model has been forced enough to avoid delayed exceptions or surprise space retention.

This preserves the existing pure style. Tests can assert both "this command compiles to the right behavior" and "this low-level model helper works." It also gives the TUI the structured result it needs: adding nodes can return the new ID, paste can return the pasted ID, and the TUI no longer has to synchronously mutate then re-read just to discover what happened.

### 3. Keep Existing Pure Helpers, But Move Them Behind Commands

Current helpers such as `moveSubtreeRelToAnchor`, `copySubtreeToClipboardWithNewIds`, and `pasteFirstClipboardEntryRelTo` should remain useful. The command interpreter can call them and compose them as endomorphisms. The point is not to rewrite all model logic immediately; the point is to stop exposing arbitrary model functions as the normal-client mutation API.

Likely code path:

1. Add command types and interpreter.
2. Add `submitCommand :: ModelServer -> ModelCommand -> IO ModelCommandResult`.
3. Implement `submitCommand` by compiling commands to endomorphisms and applying them inside the server transaction.
4. Keep `modifyModelOnServer` temporarily for privileged/internal code.
5. Port quick-add, move, refile, clipboard, and attr edits one cluster at a time.
6. Delete or de-emphasize `modifyModelOnServer` as a normal-client path once the UI no longer needs it.

### 4. Upgrade `ModelServer`

`ModelServer` should become the sole write coordinator for local clients:

- Own the current model.
- Accept structured commands from normal clients, including the TUI.
- Compile commands into internal endomorphisms.
- Execute compiled endomorphisms atomically.
- Allocate timestamps and other server-owned metadata inside command execution.
- Persist snapshots or events.
- Publish richer update messages.
- Track a monotonic revision number.

`MsgModelUpdated` should probably grow into something like:

```haskell
data ModelUpdate = ModelUpdate
  { muRevision :: ModelRevision
  , muSummary :: ModelUpdateSummary
  , muAffectedIds :: Set EID
  }
```

For Brick, this can still be sent through `BChan`. For other clients, it can be exposed as SSE, websockets, a pipe protocol, MCP resource updates, or whichever API shape wins later. The TUI should still go through `submitCommand`; being same-process should only make transport cheap, not give it a different behavioral API.

### 5. Split TUI Startup From Service Startup

Today `Srtd.Main` starts the model server and saver directly. Future executables could be:

- `srtd`: TUI client. Starts or connects to a local service.
- `srtd daemon`: local service owning the model file.
- `srtd add`: command-line capture client.
- `srtd query`: CLI query/export client.
- `srtd mcp`: MCP server, either owning the service or connecting to it.

First implementation can be simple: the TUI still starts or connects to an in-process `ModelServer`, but its Brick components submit structured commands like any other normal client. Later, a daemon can become the normal owner of the model file and storage.

### 6. Make Storage Explicit

Add something like `Srtd.Store`:

```haskell
data Store = Store
  { storeLoad :: IO DiskModel
  , storeSaveSnapshot :: DiskModel -> IO ()
  , storeAppendEvent :: ModelEvent -> IO ()
  }
```

Early version:

- Keep writing `srtd.json`.
- Add atomic write via temp file + rename.
- Add a lock file around writes.
- Add backups before writes.

Later version:

- SQLite tables for snapshots, events, metadata, clients, and sync state.
- JSON remains an export/import format.

## Protocol and Client Approaches

### One-Shot CLI With File Lock

This is the smallest step for quick-add.

Pros:

- Easy to build.
- No daemon lifecycle.
- Works in shell scripts and launchers.

Cons:

- If the TUI already has stale state in memory, external writes to `srtd.json` will be missed unless the TUI can reload/merge.
- File locking gets subtle across platforms and network drives.
- Not enough for live MCP collaboration.

Recommendation: do not choose this as the main architecture. It is useful only as a short bridge if daemon work is delayed. The preferred design is one process owning the file.

### Local Daemon / Service

A local service owns all writes. Clients connect to it.

Pros:

- Cleanest mental model for multiple clients.
- Prevents file clobbering.
- Natural place for MCP, notifications, and subscriptions.
- Lets TUI become "just another client" without losing its richness.

Cons:

- Requires lifecycle management.
- Needs connection discovery.
- Needs security decisions even on localhost.
- More moving pieces when the user just wants to run the TUI.

This is the preferred medium-term design.

### Embedded Server In The TUI

The TUI starts a local socket/http server while running.

Pros:

- Simple for "quick-add while TUI is open."
- No separate daemon.

Cons:

- External tools only work when the TUI is running.
- Weird ownership if another client wants to run first.
- Harder to make MCP independent.

This can be a pragmatic first slice but should not become the final architecture by accident. If used, it should still expose the same command API the daemon would expose later.

### SQLite Instead Of JSON

SQLite could store snapshots, events, and metadata.

Pros:

- Transactions, locks, indexes, and concurrent readers.
- Easier query surfaces for external tools.
- Better for event history and sync metadata.

Cons:

- The tree model is not naturally relational.
- Migrations become real.
- JSON is easy to inspect, copy, and recover.

Reasonable compromise: keep the canonical in-memory model as Haskell data, store snapshots/events in SQLite, and keep JSON export/import.

### Event Sourcing

Store a sequence of events and periodically snapshot.

Pros:

- Natural undo/redo and audit trail.
- Great for sync/conflict reasoning.
- Lets agents create proposal branches or patch sets.

Cons:

- More complex than the app currently needs.
- Events need stable schemas.
- Bugs in old event replay can haunt future versions.

Recommendation: design commands so they can produce events, but do not require full event sourcing in the first refactor.

### MCP Server

MCP should be a client/protocol adapter over the command/query API, not a second implementation of task behavior.

Useful MCP tools:

- `quick_add`
- `create_task`
- `update_task`
- `move_task`
- `search_tasks`
- `get_task`
- `get_subtree`
- `list_next_actions`
- `list_inbox`
- `propose_changes`

Useful MCP resources:

- Current inbox.
- Current next-action view.
- A project subtree.
- A compact schema/reference guide for agents.

The MCP surface should probably start conservative: read/search plus quick-add, then direct mutation once command validation, logging, and permissions feel solid.

### Haskell Libraries and Tooling To Investigate

- `stm`, `async`: already used; still good for the in-process service.
- `sqlite-simple`, `persistent`, or `beam`: possible SQLite layers.
- `warp`, `wai`, `servant`, or `scotty`: HTTP API options.
- `network`, `network-simple`, or Unix domain socket libraries: local socket API.
- `fsnotify`: detect external file changes if JSON remains writable by other processes.
- `ekg`, `co-log`, or richer logging: optional service observability.
- `optparse-applicative`: already used and good for CLI expansion.
- MCP Haskell support: investigate current state before committing; if the ecosystem is thin, a small JSON-RPC server over stdio may be simpler than waiting for a mature library.

## Blockers and Footguns

### Opaque Model Mutations As Client API

`modifyModelOnServer :: ModelServer -> (Model -> Model) -> IO ()` is the big blocker if treated as the public write API. It is convenient inside Haskell and should remain available as an internal implementation tool while commands are migrated. It should not be the normal path for TUI, CLI, MCP, or other clients.

### Synchronous UI Assumptions

Some TUI flows assume the model update happens immediately and then reload the model. A real service may make command submission async or return richer results. The TUI should move toward "submit command, receive result/update, reconcile focus."

### Transaction-Local Effects

UUIDs do not generally need synchronization; the point of UUIDs is that clients can safely allocate them without coordinating. Still, some command-related effects and observations should happen transaction-locally:

- The command result should be derived from the same state transition the server applied.
- Timestamps and revision numbers should be server-owned.
- Any operation that depends on the current model, such as "paste first clipboard entry," should choose that payload inside the transaction.
- UUID allocation can be client-side or server-side, but the result should clearly report the IDs that were actually inserted.

### JSON Rewrite Semantics

Rewriting the whole model is simple but risky with multiple writers. Atomic temp-file rename, locks, and backups are necessary if external processes can touch the file. A daemon or SQLite avoids much of this.

Recommendation: do not let multiple processes touch the file. A centralizing daemon or service should own storage. Atomic writes and backups are still important, but they become internal safety measures rather than a multi-writer coordination scheme.

### Lazy Evaluation

The architecture doc already notes that updates are lazy. In a service architecture, this can move expensive work to surprising places or hide exceptions until after a command was "accepted." Consider forcing enough of the new model before publishing success.

This should be handled deliberately. Separately, the known long-running memory/space leak may be related to surprising laziness in model updates or tree transformations. The service refactor is not a substitute for diagnosing that leak, but it is a good chance to put stricter evaluation boundaries around accepted commands.

### Derived Data Consistency

Derived attrs are recomputed on model updates. External clients should not write derived fields. Storage and protocol schemas should clearly separate physical attrs from derived/query results.

### Conflict Semantics

If two clients edit the same node, "last writer wins" may be acceptable for capture but terrible for careful planning. Commands should probably include optional preconditions:

- Expected model revision.
- Expected node revision.
- Expected parent.
- Expected attr value.

### Authorization and Privacy

AI agents should not automatically get all tasks. Even local MCP needs a small capability model:

- Read-only vs write.
- Capture-only.
- Read-from-subtree.
- Read/write-to-subtree.
- Require confirmation for destructive operations.
- Redact private metadata or projects.

Do not build full users/roles/team authorization. This is still a single-user tool. Capabilities are the right abstraction: narrow enough to protect private subtrees and make MCP safer, simple enough not to turn `srtd` into enterprise middleware. If that later enables limited multi-user interaction, fine; it is not a primary goal.

### Schema Stability

Once CLI/MCP protocols exist, changing command JSON becomes a compatibility issue. Use explicit versions early.

### Error Design

External clients need real errors, not silent no-ops:

- ID not found.
- Invalid move into own descendant.
- Permission denied.
- Revision conflict.
- Parse failure.
- Unsupported command version.

The TUI can still treat many of these as "cancel and reload," but the service API should be honest.

### Protocol Drift

Do not let the MCP server, CLI, and TUI each grow their own definitions of "create task" or "move task." They should all call the same command interpreter.

### Daemon Lifecycle

A daemon introduces boring-but-real issues:

- Is it started automatically?
- Where is the socket?
- What happens after crash?
- How does it find `srtd.json`?
- Can two daemons start in the same directory?
- How does the TUI behave if the daemon version differs?

### Human Factors

External interfaces will make capture easier. That can make inbox rot worse. The architecture should support review and processing workflows, not only more ways to dump tasks into `INBOX`.

This is real but separate from the command/service refactor. Keep it in mind, but do not block the architecture work on solving inbox processing.

## Suggested Roadmap

### Slice 1: Command API Inside The Existing TUI

- Add `ModelCommand`, `ModelCommandResult`, and `ModelCommandError`.
- Add `submitCommand` to `ModelServer`.
- Implement command compilation to internal endomorphisms.
- Port quick-add to the command API.
- Keep old `modifyModelOnServer` during migration.
- Add tests for command execution.

### Slice 2: TUI As A Normal Client

- Port the Brick model mutations to `submitCommand` cluster by cluster.
- Keep same-process submission as a no-op transport.
- Make sync/focus behavior use `ModelCommandResult` and richer update messages.
- Keep arbitrary endomorphism submission only for internal/debug paths.

### Slice 3: Local Service / Daemon

- Let one process own the model and storage.
- Expose a simple local protocol.
- Make TUI and CLI connect to it.
- Publish revisioned updates to clients.

### Slice 4: CLI Quick Add

- Add `srtd add`.
- Use the same command type library and submission functions.
- Prefer connecting to the daemon instead of writing the file directly.

### Slice 5: MCP Read/Search/Capture

- Implement MCP against the service API.
- Start with read/search and `quick_add`.
- Add direct mutations only after logging, permissions, and error behavior feel good.

### Slice 6: Events, Undo, Sync

- Introduce durable event logging if still valuable.
- Add undo/redo and history views.
- Explore multi-machine sync with explicit conflict handling.

This probably requires a deeper data model change, perhaps toward event sourcing. Treat it as related but separate from the command API question.

## Open Questions

- Should JSON remain the canonical storage format, or become an export format?
- Should the daemon be optional forever, or should it become the normal runtime?
- How much should AI agents be allowed to mutate directly without confirmation?
- Do commands need per-node revisions immediately, or is whole-model revision enough at first?
- Should external clients see only physical attrs, or also derived/query projections?
- How should notes and attached text files fit into command/event history?
- Collaboration with other humans is not a real product goal right now. Keep it generally in scope as an architecture sanity pressure, but do not build actual collaboration features.

## Provisional Recommendation

Build the command boundary first. It is the least flashy work and the most enabling. Normal clients, including the TUI, should submit structured commands. The model server should compile those commands into pure endomorphisms and execute them internally. Once model mutations are explicit and result-bearing at the boundary, `srtd` can grow outward in several directions without duplicating behavior: CLI quick-add, daemon, MCP server, notifications, import/export, and eventually sync.

The current `ModelServer` is a good seed for this. It already centralizes access; it just needs to stop accepting arbitrary functions as its normal-client write protocol.
