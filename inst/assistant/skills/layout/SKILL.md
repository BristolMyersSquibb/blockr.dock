---
name: layout
description: >-
  How to arrange the dock's views:
  the views and the folders that group them in the navigation,
  the grid of splits, tab groups and rails that places the panels of a view,
  and the dock's commands that change them,
  from `blockr.dock-add_view` to `blockr.dock-set_grid`.
  Read it before you add, rename, move or remove a view or a folder,
  add, move, resize or show a panel, or set the sections of a card.
---

# Layout

The dock shows the board in views, and the user picks one in the navigation.
Folders group the views there.
Each view has a grid, which places its panels.
A panel shows the card of a block or the UI of a plugin.

The dock keeps all of this in its plugin state.
The tool `get_plugin_state` gives that state for the plugin `blockr.dock-dock_plugin`.
Read it before you change a view.
The commands name views, folders and panels as the state holds them.

Change the state with the dock's commands, not with `blockr.core-set_plugin_state`.
The dock's commands check what they are given.
They write each grid in a form that the dock draws as it is.
This skill says how the commands fit together.
The tool `describe_command` gives the args of each in full.

## The dock's state

The state has four parts:

- The part `views` holds each view by its id, with its `name`, the `folder` it sits in, its place in that folder and its `grid`.
- The part `folders` holds each folder by its id, with its `name`, the `folder` it sits in and its place there.
- The part `active` holds the id of the active view, which the dock shows.
- The part `visible` holds, by block id, the sections that a block's card shows, for each card that hides one.

A view or a folder is known by its id, which never changes.
Its name is only a label.
Two views may have the same name, and renaming a view changes nothing else.
A command names a view or a folder by its id, never by its name.

## Views and folders

The command `blockr.dock-add_view` adds a view under a new id and makes it the active view.
Each of its args `name`, `folder`, `before` and `grid` may be left out.
Without a name, the view is called "View N".
Without a grid, it shows nothing.

A folder groups views and other folders, at any depth.
It only orders the navigation.
A view shows its own grid, whatever folder it sits in.
The command `blockr.dock-add_folder` adds a folder.

A view or a folder goes into a folder, or to the top of the navigation.
The args name the top as `null`.
It goes before the view or folder that `before` names, as `{ "view": id }` or `{ "folder": id }`.
Without `before`, it goes at the end of that folder.
Mind the names of the args when you move one.
The command `blockr.dock-move_view` takes the folder that the view goes into as `folder`.
The command `blockr.dock-move_folder` takes the folder that it moves as `folder`, and the folder that it goes into as `into`.

The command `blockr.dock-remove_view` removes a view, but none of its blocks.
It refuses to remove the last view.
The command `blockr.dock-remove_folder` removes a folder and moves what it holds up into its place, so it removes no view.
The commands `blockr.dock-rename_view` and `blockr.dock-rename_folder` take a name that holds a character other than a space.

## Panels

A panel is named by what it shows:

- The card of a block is `{ "block": id }`, by the block's id.
- The UI of a plugin is `{ "plugin": id }`, by the id of the plugin's class.

The dock draws the panel of a plugin only where the plugin's class has a UI.
A view shows a block or a plugin at most once.
The same block may show in several views, so do not refuse a request for that.
Removing a block takes its panel out of every view in the same step, so it needs no command of the dock's.

A block that a command adds, such as `blockr.core-add_block`, gets a panel in the active view at once, at the default place.
The later commands of the same edit see it there.
So to place a new block elsewhere in the active view, move its panel with `blockr.dock-move_panel`.
The command `blockr.dock-add_panel` refuses it there, since the view shows the panel already.

The default place of a panel is a new tab, which its tab group then shows.
The tab goes in the last tab group, in reading order, that holds only the panels of blocks.
Where no tab group holds only the panels of blocks, as in a view that shows nothing, the panel gets a new tab group at the end of the top split.

## Grids

A grid holds nested splits of tab groups, and a rail on either edge of the view:

```json
{
  "orientation": "horizontal",
  "children": [
    { "panels": [{ "block": "data" }, { "block": "head" }], "active": { "block": "data" } },
    {
      "children": [
        { "panels": [{ "block": "plot" }], "active": { "block": "plot" } },
        { "panels": [{ "block": "table" }], "active": { "block": "table" } }
      ],
      "sizes": [0.6, 0.4]
    }
  ],
  "sizes": [0.3, 0.7],
  "rails": {
    "left": {
      "panels": [{ "plugin": "blockr.core-edit_board_plugin" }],
      "active": { "plugin": "blockr.core-edit_board_plugin" },
      "collapsed": false,
      "size": 500
    }
  }
}
```

This view shows a tab group of `data` and `head` in its left 30%, with `data` in front.
The other 70% shows `plot` above `table`.
Core's Edit board plugin sits on a rail at the left edge of the view.

A grid follows these rules:

- The top split names its `orientation`.
  With "horizontal", its children sit side by side, and with "vertical", one above the other.
- Each split inside divides the other way from the split that holds it.
  So only the top split names an orientation.
- A split holds its `children`, each a split or a tab group, and their `sizes`.
- Each child takes a share of its split, above 0.
  The shares of a split sum to 1, as `[0.5, 0.25, 0.25]` and `[0.34, 0.33, 0.33]` do.
  A grid with `[0.33, 0.33, 0.33]` is refused.
- A tab group is always an object, even for a single panel.
  It holds its `panels`, in the order of their tabs.
  Its `active` is the panel that it shows, one of its own.
- A rail is not a child of a split, and it takes no share.
  It holds `panels` and `active` as a tab group does.
  Its `collapsed` says whether it shows its tabs alone.
  Its `size` is its width in pixels while it is open, 500 by default.
- A grid places each panel once, in one tab group or on one rail.
- No split, tab group or rail is empty.
  Only the top split may be empty, in the grid of a view that shows nothing: `{ "orientation": "horizontal", "children": [], "sizes": [] }`.
- A grid takes no other key, so a split inside takes no `orientation`.
- A grid names only blocks that the board holds, or that an earlier command of the same edit adds.

A refused edit names each rule that its grid breaks, and where in the grid.

## Placing a panel

The command `blockr.dock-add_panel` places a panel in a view that does not show it.
The command `blockr.dock-move_panel` moves a panel within a view that shows it.
Both take where the panel goes:

1. With `rail`, "left" or "right", the panel goes on the rail at that edge of the view, as its last tab, which the rail shows.
   The view gets a rail there where it has none.
   The arg `rail` wins over `near` and `side`.
2. With `near`, a panel of the view, the panel goes by the tab group of `near`, on the `side` given:
   - With "within", or no `side`, it goes in that tab group, as a new tab that the group shows.
   - With "left", "right", "above" or "below", it goes in a new tab group on that side of the tab group.
   - Where `near` sits on a rail, the panel goes onto that rail, and `side` can only be "within".
3. With a `side` other than "within" and no `near`, the panel goes in a new tab group at that edge of the whole grid.
4. With none of them, the panel goes to the default place.

The panel that `near` names may be one that an earlier command of the same edit placed.
Do not mix up `side` and `rail`.
A side is a direction from the tab group of `near`, inside the splits.
A rail sits on an edge of the whole view, outside them.
To bring a panel off a rail into the splits, move it with a `near` that sits in the splits.

A new tab group beside another gets a share that the command picks.
Where its share matters, follow with `blockr.dock-resize_panel`.

The command `blockr.dock-remove_panel` takes a panel out of one view, as closing its tab does.
It removes no block, and the other views keep their panels of it.
A block that no view shows stays on the board.

## Sizes

The command `blockr.dock-resize_panel` sets the size of the tab group or rail that holds a panel:

- For a tab group, `size` is its share of the split that holds it, above 0 and below 1.
  The other children of that split share the rest in proportion.
  The command refuses a tab group that fills its split alone, since its share stays 1.
- For a rail, `size` is its width in pixels while it is open.

The share is that of the split that holds the tab group directly.
Take a column that holds two tab groups, one above the other.
To widen that column, resize a tab group that sits beside it, or set the whole grid.

## Whole grids

The command `blockr.dock-add_view` takes the grid of the new view.
The command `blockr.dock-set_grid` replaces the grid of a view whole.
So the view loses every panel that the new grid leaves out.
Use a whole grid to arrange a view anew.
For a change to one or two panels, use the commands above, which keep the rest of the grid as it is.

A view that `blockr.dock-add_view` adds gets an id that the edit cannot know in advance.
So no later command of that edit can name the new view.
To build a view of new blocks in one edit:

1. Add the blocks with `blockr.core-add_block`.
   The dock places their panels in the active view.
2. Add the view with `blockr.dock-add_view` and a grid that places the new blocks.
   The new view becomes the active view.
3. Where the blocks should show in the new view alone, take their panels out of the view that was active before, with `blockr.dock-remove_panel`.

A block that the edit adds after the view goes into the new view instead, at the default place.
The new view is then the active view.

## Showing a view or a panel

Four commands change only what the user looks at:

- The command `blockr.dock-activate_view` makes a view the active one.
- The command `blockr.dock-activate_panel` has the tab group or rail that holds a panel show it, as clicking its tab does.
  It leaves a collapsed rail collapsed.
- The command `blockr.dock-show_panel` shows a panel in the active view.
  It brings the panel's tab to the front, and opens its rail where the rail is collapsed.
  Where the view does not show the panel, it places the panel at the default place.
  It never switches views.
  Use it to show the user a block that you changed, where the active view hides it behind another tab or on a collapsed rail.
- The command `blockr.dock-collapse_rail` collapses or opens the rail on an edge of a view.
  The board option `sync_rails` is on unless it is set to `FALSE`.
  While it is on, the command collapses or opens the rail on that edge of every view that has one.

These four take no undo step.
An edit made of them alone takes none either, so the user's Undo and `undo_edit` pass over it.
Every other command of the dock's is one undo step.

## Card sections

The command `blockr.dock-set_sections` sets the sections that a block's card shows, in every view.
Its arg `sections` takes `["inputs", "outputs"]` for both, one of them alone, or `[]` for neither.
A card shows both unless this command sets it otherwise.

## A locked board

On a locked board, the user can still switch views, bring a tab to the front, and open or collapse a rail.
So can you.
The command `blockr.dock-show_panel` then only brings forward a panel that the active view shows.
No command adds, renames, moves or removes a view or a folder.
No command adds or removes a panel, or sets the sections of a card.
Moving a panel works only where the deployment sets the blockr option `lock_rearrange` to `FALSE`.
Resizing one works only where it sets the blockr option `lock_resize` to `FALSE`.
The command `blockr.dock-set_grid` refuses a grid that changes more than these leave open.
The command `blockr.core-set_plugin_state` is refused on a locked board too.
So tell the user what the lock keeps, rather than trying another command.
