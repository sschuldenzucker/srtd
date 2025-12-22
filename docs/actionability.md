
# Actionability

## Actionability and Status

_Status_ is a property of each item and describes the "type" of item and (equivalently) how actionable it is in isolation.

_Actionability_ is a property of the item _in its context_ and describes how actionable it is given that context. For example, a Project may have Next actionability only because it has a Next child and a Next child may have Someday actionability because it is below a Someday item. See below for details.

## Global and Local Actionability

There are generally two notions of actionability:

1. _Global_ actionability is only based on the children of an item and the item itself and is independent of the currently viewed subtree.
2. _Local_ actionability also takes the ancestors between the currently selected root and the item into account. Local actionability is always <= global actionability b/c ancestors may block _visibility_.

For example, a Next item below a Someday item (common for paused projects) has _global_ actionability = Next but _local_ actionability = Someday as long as the root is strictly above the Someday ancestors. If the root is below or equal to the Someday ancestor, it doesn't apply anymore, and local actionability of the Next item becomes Next.

The purpose for this split design is to let the user plan even paused projects simply by hoisting into them. Without the split design, everything would show up as (say) Someday actionable just b/c the parent is Someday, which isn't very useful.

## Visibility / Transparency

To decide actionability, we consider some statuses (specifically None, Project, and in a limited fashion Open) as transparent. They do not limit actionability of their children.

Transparency rules affect both global and local actionability. For example:

- Global actionability: A Project's actionability depends on the actionability of its children.
- Local actionability: A Next child below a Project remains Next.

> [!SOMEDAY] These are actually pretty different things. The fact that I can use the same code to implement them suggests that there is some simpler structure to describe "transparency" and that may be useful to unify code.

The main site of implementation for these rules is `applyActionabilityTransparency` in `Srtd.Attr`, and some rules unfortunately also need to be replicated into `sacForParent`.

We call an item _visible_ if its ancestors are all transparent to its status.

## Top Bar and Bottom Bar

The top bar and bottom bar show, respectively, statistics for the number of items by bottom and actionability _below_ (1) the current root and (2) the currently selected item.

Consistency condition: The counts in the bottom bar are such that, (1) if we hoist into the selected item, then these values move into the top bar for the root. And (2) these counts also correctly reflect the item counts in the respective views.

> [!SOMEDAY] Consistency condition (1) is guaranteed by the data model, but (2) is currently *not*. That's not so good. 

Internally, the counts shown in the bars are _global_ properties: they are not recomputed per root choice. This is possible because the statement "if this node is selected as root, we'll have the following local statistics" is itself a global property (doesn't depend on which root is currently viewed).

The counting rules here are implemented in the methods of `StatusActionabilityCounts` in `Srtd.Attr` and unfortunately reproduce some of the visibility rules.

> [!SOMEDAY] It would be nice, and much more robust, to unify these rules.

## Stalled Projects

Conceptually, a _stalled project_ is one where the following hold:

1. The project doesn't have any Next or Waiting ancestors (i.e., has global actionability worse than Waiting)
2. The project is visible in the sense that it doesn't have any ancestors that make it non-actionable (like Next or Someday)
3. The project doesn't have a parent project that is Next-actionable.

On condition 1., note that Waiting projects are _not_ considered stalled: they are _blocked_, which is a different thing. Stalled projects don't make progress b/c they are (usually) underspecified or unclear. We can usually fix this ourselves. Blocked projects don't make progress b/c we are waiting for input from other people.

Condition 3. is so that the stalled project actually blocks something: either itself (if it is a toplevel project) or a parent project that cannot make progress in some other way. We are intentionally strict here to limit the number of false positives of stalled projects shown to the user.

> [!SOMEDAY] Condition 3. is open to change.
>
> It is not clear to me at this point what the user really wants: it may well turn out that stalled projects are generally problematic even if the parent is generally unblocked.
>
> To be revisited.

The main sites of implementation for this are, in `Srtd.Attr`, `isGlobalStalledProject`, `isLocalStalledProject`, and `sacForParent`. Unfortunately, the logic has to be replicated into `sacForParent`.

SOMEDAY that replication is painful. We may be able to unify using more detailed context to the `is*StalledProject` methods.
