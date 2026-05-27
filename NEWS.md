# blockr.dock (development version)

* Locked-mode hardening:
  * Server-side trust boundary: every state-mutating `observeEvent` in
    the package now gates its `eventExpr` with `req_unlocked()`, so a
    forged `Shiny.setInputValue` cannot bypass UI hides (#127).
  * New exported helpers `is_dock_locked()` and `req_unlocked()` give
    extension authors the same primitives. `blockr.dock` does not
    auto-hide extensions in locked mode; each extension picks its own
    behaviour (see `?dock-locked`).
  * Empty views in locked mode now show a read-only placeholder
    instead of the "Add panel" call-to-action (#136).
  * The board-options accordion is hidden from the settings sidebar in
    locked mode; the gear button stays for read-only access to the
    generated-code export (#135).

# blockr.dock 0.1.1

* Added prepend block action.
* Define multi-view boards with `dock_layouts()`:

  ```r
  layout = dock_layouts(
    Analysis = list("block_1", "block_2"),
    Overview = list("dag_extension")
  )
  ```

  Mark a view as initially active by tagging its spec via `dock_view()`:

  ```r
  layout = dock_layouts(
    Analysis = list("block_1", "block_2"),
    Overview = dock_view("dag_extension", active = TRUE)
  )
  ```

  If no view is tagged, the first one is used.

  In `new_dock_board`, the layout now defaults to `dock_layouts(Page = default_view_grid(blocks, extensions))`, so it create a single page dashboard with a default grid when nothing is specified by the user.

# blockr.dock 0.1.0

* Initial CRAN submission
