# Navbar Architecture: Where Should It Live?

## Context

The navbar provides workflow management UI:
- Save/load workflows
- Recent workflows dropdown
- Editable workflow title
- New workflow button
- Settings toggle (opens offcanvas)
- Code button (triggers generate_code plugin)
- User avatar with initials

Currently, the navbar lives entirely in `blockr.dock` but relies heavily on `blockr.session` for the session management backend.

## Current Implementation

### blockr.dock
- `R/navbar-ui.R` - UI component
- `R/navbar-server.R` - Server logic
- Calls into `blockr.session:::` functions for save/load

### blockr.session
- `upload_board()` - Save workflow to pins backend
- `download_board()` - Load workflow from pins backend
- `pin_list()` / `pin_versions()` - List saved workflows
- `manage_session` plugin - Sidebar UI for save/restore (separate from navbar)

## Architectural Options

### Option 1: Keep in blockr.dock (Current)

```
blockr.dock
├── navbar-ui.R      (full navbar UI)
├── navbar-server.R  (calls blockr.session:::functions)
```

**Pros:**
- Simple, works now
- Navbar is visually tied to dock layout (sits above dockViewR)
- Settings button toggles dock-specific offcanvas
- Code button triggers dock-specific plugin

**Cons:**
- Non-dock boards cannot reuse this UI
- Session UI logic duplicated between navbar and sidebar panel
- Tight coupling to blockr.session internals (`:::`)

### Option 2: Move Navbar to blockr.session

```
blockr.session
├── navbar-ui.R      (session navbar UI)
├── navbar-server.R  (session navbar server)

blockr.dock
├── (imports and uses session navbar)
```

**Pros:**
- Session management UI lives with session management logic
- Could work with any board type (not just dock)
- Single source of truth for session UI

**Cons:**
- Plugins currently render in sidebar, not as top navbar
- Would need new plugin architecture for "chrome" UI (navbar, footer, etc.)
- Some navbar features are dock-specific (settings toggle, code button)
- blockr.session would need to know about dock-specific features

### Option 3: Split Approach (Recommended)

```
blockr.session
├── session-navbar-ui.R      (save, load, workflows, title)
├── session-navbar-server.R  (session logic)
├── exports: session_navbar_ui(), session_navbar_server()

blockr.dock
├── navbar-ui.R      (wraps session navbar, adds dock buttons)
├── navbar-server.R  (wraps session navbar, adds dock logic)
```

**Pros:**
- Clean separation of concerns
- Session-related parts are generic and reusable
- Dock-specific parts (settings, code) stay in blockr.dock
- Other board types could use session navbar independently
- No `:::` calls needed - use exported functions

**Cons:**
- More coordination between packages
- Need to design a clean API boundary
- Two packages to update for navbar changes

### Option 4: Navbar as Extension

```
blockr.dock
├── ext-navbar.R     (new_navbar_extension())
├── Similar to new_dag_extension()
```

**Pros:**
- Opt-in rather than always present
- Follows existing extension pattern
- Could have multiple navbar variants

**Cons:**
- Extensions typically render in panels, not as app chrome
- Would need to change extension architecture
- Doesn't solve the blockr.session coupling issue

## Recommendation

**Option 3 (Split Approach)** provides the best balance:

1. **blockr.session exports:**
   - `session_navbar_ui(id)` - UI for save/load/workflows/title section
   - `session_navbar_server(id, board, backend)` - Server logic
   - These are generic, work with any board type

2. **blockr.dock wraps and extends:**
   - Imports session navbar components
   - Adds dock-specific buttons (settings toggle, code)
   - Handles dock-specific layout (above dockViewR)

3. **Migration path:**
   - Extract session-related navbar code to blockr.session
   - Keep dock-specific code in blockr.dock
   - Update blockr.dock to import from blockr.session

## API Design Sketch

### blockr.session

```r
#' Session navbar UI component
#' @param id Namespace ID
#' @param show_save Show save button (default TRUE
#' @param show_workflows Show workflows dropdown (default TRUE)
#' @param show_title Show editable title (default TRUE)
#' @param show_new Show new button (default TRUE)
session_navbar_ui <- function(id,
                              show_save = TRUE,
                              show_workflows = TRUE,
                              show_title = TRUE,
                              show_new = TRUE) {

  # Returns tagList with session-related navbar elements
}

#' Session navbar server
#' @param id Namespace ID
#' @param board Reactive values object containing board state
#' @param backend pins board for storage (default from blockr_option)
session_navbar_server <- function(id, board, backend = NULL) {
  # Handles save, load, title edit, new workflow
  # Returns list with reactive values for status updates
}
```

### blockr.dock

```r
navbar_ui <- function(id) {
  ns <- NS(id)

  tags$nav(
    class = "blockr-navbar",
    # Left section - from blockr.session
    tags$div(
      class = "blockr-navbar-left",
      blockr.session::session_navbar_ui(ns("session"))
    ),
    # Right section - dock-specific
    tags$div(
      class = "blockr-navbar-right",
      # Settings button (dock-specific)
      # Code button (dock-specific)
      # Avatar
    )
  )
}
```

## Open Questions

1. Should the navbar be opt-in via a plugin/option, or always present for dock boards?
2. How should the navbar communicate with existing `manage_session` sidebar UI? Should they share state?
3. Should `preserve_board` plugin be deprecated in favor of navbar-based save/load?
4. How to handle the "Code" button for non-dock boards that may not have `generate_code` plugin?

## Next Steps

1. Design the API boundary between blockr.session and blockr.dock
2. Extract session-related code from navbar-server.R
3. Create exported functions in blockr.session
4. Update blockr.dock to use the exports
5. Test with both dock and non-dock boards
