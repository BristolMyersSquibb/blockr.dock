# Proposal: Remove Bootstrap Dependencies from blockr.dock

## Summary

Remove `bslib`, `shinyWidgets`, and `shinyjs` from blockr.dock, replacing them with custom CSS and vanilla JavaScript. This gives us full control over styling, reduces bundle size, and eliminates version conflicts.

## Current State (Production)

**Dependencies to remove:**
- `bslib` - Bootstrap 5 wrapper, provides `accordion()`, `popover()`, `useBusyIndicators()`
- `shinyWidgets` - Provides `checkboxGroupButtons()`, `colorPickr()`
- `shinyjs` - Provides `useShinyjs()`, `show()`, `hide()`, `toggle()`

**What they provide:**
| Package | Functions Used | Replacement |
|---------|---------------|-------------|
| bslib | `accordion()`, `accordion_panel()`, `accordion_panel_set()` | Custom R + JS |
| bslib | `popover()` | Custom R + CSS (hover-based) |
| bslib | `useBusyIndicators()` | Remove (not essential) |
| shinyWidgets | `checkboxGroupButtons()` | Custom `toggle_buttons()` + Shiny input binding |
| shinyWidgets | `colorPickr()` | Native HTML5 `<input type="color">` |
| shinyjs | `show()`, `hide()`, `toggle()` | CSS classes + `sendCustomMessage()` |

## Proposed Architecture

```
blockr.dock/
├── R/
│   └── utils-ui.R          # Custom components: accordion(), popover(), toggle_buttons(), color_input()
└── inst/assets/
    ├── css/
    │   └── blockr-dock.css # Complete design system with CSS variables
    └── js/
        └── blockr-components.js  # Accordion, modal, offcanvas, dropdown handlers + Shiny bindings
```

**Key principle:** `suppressDependencies("bootstrap")` in `blockr_app_ui()` prevents Bootstrap CSS/JS from loading, then our custom assets take over.

## Pros

| Benefit | Description |
|---------|-------------|
| **Smaller bundle** | ~200KB less CSS/JS to load (no Bootstrap 5) |
| **No version conflicts** | bslib updates won't break our styling |
| **Full control** | Every pixel is ours to customize |
| **Faster iteration** | Change CSS directly, no fighting Bootstrap specificity |
| **Cleaner code** | CSS variables, consistent naming, no `!important` hacks |
| **Better performance** | Less CSS to parse, no unused Bootstrap utilities |
| **Simpler debugging** | One source of truth for styles |

## Cons

| Drawback | Mitigation |
|----------|------------|
| **More code to maintain** | Well-structured, documented CSS/JS (~500 lines each) |
| **No Bootstrap ecosystem** | We only use a few components anyway |
| **Initial development time** | ~2 days for full implementation |
| **Browser testing needed** | Use standard CSS, test major browsers |
| **Team learning curve** | Document patterns, use CSS variables consistently |

## Implementation Plan

### Phase 1: Foundation (2-3 hours)
1. Add `suppressDependencies("bootstrap")` to `blockr_app_ui()`
2. Add Bootstrap shim (`window.bootstrap = {...}`) to satisfy bslib version checks
3. Ensure base layout still works

### Phase 2: Replace shinyWidgets (2 hours)
1. Create `toggle_buttons()` with Shiny input binding
2. Create `color_input()` using native HTML5 color picker
3. Remove shinyWidgets from DESCRIPTION

### Phase 3: Replace shinyjs (1 hour)
1. Replace `show()`/`hide()` with CSS class toggling via `sendCustomMessage()`
2. Remove shinyjs from DESCRIPTION

### Phase 4: Replace bslib Components (3-4 hours)
1. Create custom `accordion()`, `accordion_panel()`, `accordion_panel_set()`
2. Create custom `popover()` (CSS hover-based)
3. Remove `useBusyIndicators()` or create simple CSS alternative
4. Remove bslib from DESCRIPTION

### Phase 5: CSS Cleanup (2 hours)
1. Ensure all Bootstrap utility classes have custom equivalents
2. Add form element font inheritance
3. Test all components

### Phase 6: Testing (2-3 hours)
1. Test all modals (add block, add link, create stack, etc.)
2. Test accordion expand/collapse
3. Test offcanvas panels
4. Test DAG extension (G6 graph positioning)
5. Test on different browsers

**Total estimated time: 1.5-2 days**

## File Changes Summary

| File | Action |
|------|--------|
| `DESCRIPTION` | Remove bslib, shinyWidgets, shinyjs from Imports |
| `R/utils-pkg.R` | Remove `@import bslib` |
| `R/utils-ui.R` | Add `accordion()`, `popover()`, `toggle_buttons()`, `color_input()` |
| `R/utils-serve.R` | Remove `useBusyIndicators()`, add `suppressDependencies()` |
| `R/plugin-block.R` | Use custom `toggle_buttons()` instead of `checkboxGroupButtons()` |
| `R/action-modal.R` | Use custom `color_input()` instead of `colorPickr()` |
| `inst/assets/css/blockr-dock.css` | Add ~200 lines for components, utilities |
| `inst/assets/js/blockr-components.js` | Add ~300 lines for accordion, modal, Shiny bindings |

## Recommendation

**Proceed with implementation.** The benefits (bundle size, control, maintainability) outweigh the costs (initial development time). The implementation is straightforward and low-risk since we're replacing external dependencies with equivalent internal code.

blockr.core remains unchanged and keeps Bootstrap for development simplicity - this change only affects blockr.dock's production UI.
