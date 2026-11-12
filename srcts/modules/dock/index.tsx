// The dock's frontend, the module blockr.dock-dock that the dock plugin's
// class names in `frontend`, whose render() core calls with the page's
// element (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#the-frontend).
// It draws the page as a React app on the one copy of React that core puts on
// the page, with the layout drawn by dockview, which its bundle holds.
// The page is an empty dockview, which holds no panel.

import { DockviewReact, themeLightSpaced } from "dockview-react";
import type { IDockviewReactProps } from "dockview-react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { FrontendModule } from "@blockr/core";

export const api = 1;

// The components that draw the content of panels, by name, of which dockview
// needs one for each panel it draws.
const panels: IDockviewReactProps["components"] = {};

function Dock() {
  return (
    <div className="blockr-dock">
      <DockviewReact components={panels} onReady={() => undefined} theme={themeLightSpaced} />
    </div>
  );
}

export default {
  render({ el }) {
    const root = createRoot(el);
    // React draws the page before render() returns.
    flushSync(() => {
      root.render(<Dock />);
    });
    return () => {
      root.unmount();
    };
  },
} satisfies FrontendModule;
