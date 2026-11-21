// The panel picker, one of the dock's pickers (./picker.tsx), which lists what
// the active view does not show, and adds the panel that the user picks to
// the view
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#a-plugins-panel).
// It lists the board's blocks that the view does not show, each by the mark of
// its class, its name and the name of its class, in the order of the board,
// as the design system lists the blocks of a board in a menu.
// After them, under "Plugins", it lists the app's plugins with a `ui` that the
// view does not show, by their names, in the order of the app's plugins.
// Its search finds them by name, by the name of a block's class and by id, so
// that the picker serves a board of many blocks.
// Today's Blockr.menu shows one only where it lists more than eight, at
// 655e323.
// Where the picker has nothing to list, it says why in place of the list.
//
// Two buttons open it: the "+" in the header of each tab group, and the "Add
// panel" button of an empty view.
// One picker serves them all.
// It opens at the button that the user pressed.
// A pick runs blockr.dock-add_panel on the active view.
// From a tab group's "+", the pick goes near the panel that the group shows,
// so the panel becomes a new tab in that group.
// From an empty view, the panel goes to the default place.
// Where the board refuses the command, core tells the user why, as an error of
// the code board_edit_failed.

import type { BlockRegistry, Board, Json, PluginClassId } from "@blockr/core";
import { useBoard } from "@blockr/core/react";
import type { ButtonProps } from "@blockr/core/react";
import { createContext, useContext, useId, useMemo, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { activeView, dockState } from "../../commands/index.ts";
import { places } from "../../state/index.ts";
import type { Panel } from "../../state/index.ts";
import { Mark } from "./card/index.ts";
import { useDock } from "./context.ts";
import { Picker } from "./picker.tsx";
import type { PickerGroup } from "./picker.tsx";

// The open panel picker.
interface Picking {
  // The button that opened it, with an id of the button's own.
  readonly id: string;
  readonly button: HTMLElement;
  // The panel near which a pick goes, the one that the group of a tab group's
  // "+" shows, or undefined for the default place.
  readonly near: Panel | undefined;
}

interface PickerState {
  readonly picking: Picking | undefined;
  readonly setPicking: (picking: Picking | undefined) => void;
}

const PickerContext = createContext<PickerState | null>(null);

function usePicker(): PickerState {
  const value = useContext(PickerContext);
  if (value === null) throw new Error("A button of the panel picker is drawn outside the picker.");
  return value;
}

// The panel picker, which the buttons among its children open.
export function PanelPicker({ children }: { readonly children: ReactNode }): ReactElement {
  const [picking, setPicking] = useState<Picking | undefined>(undefined);
  const state = useMemo(() => ({ picking, setPicking }), [picking]);
  return (
    <PickerContext value={state}>
      {children}
      {picking === undefined ? null : (
        <PanelMenu
          picking={picking}
          onClose={() => {
            setPicking(undefined);
          }}
        />
      )}
    </PickerContext>
  );
}

// The props of a button of core's React SDK that opens the panel picker.
// The function `near` gives the panel near which a pick goes, or undefined for
// the default place, as the user opens the picker.
// Safari does not focus a button that the user clicks, so the button takes the
// focus as it opens the picker, for React Aria to give it back as the picker
// closes.
export function usePickerButton(near: () => Panel | undefined): ButtonProps {
  const { picking, setPicking } = usePicker();
  const id = useId();
  return {
    "aria-expanded": picking?.id === id,
    onPress: (event) => {
      const button = event.target;
      if (!(button instanceof HTMLElement)) return;
      if (event.pointerType !== "keyboard") button.focus({ preventScroll: true, focusVisible: false });
      setPicking({ id, button, near: near() });
    },
  };
}

// The groups of the panel picker on a board, where the dock draws the panels
// of the plugins given: those that the active view does not show, none where
// there is no active view.
// A row's key names its block or its plugin, as "block:a", so that no block's
// row shares its key with a plugin's.
function pickerGroups(
  board: Board,
  registry: BlockRegistry,
  drawn: ReadonlyMap<PluginClassId, string>,
  pick: (panel: Panel) => void,
): PickerGroup[] {
  const view = activeView(board);
  if (view === undefined) return [];
  const blocks = Object.entries(board.blocks).flatMap(([block, held]) => {
    if (places(view.grid, { block })) return [];
    const entry = registry[held.registry_id];
    return [
      {
        key: `block:${block}`,
        label: held.name,
        lead: <Mark entry={entry} size={24} />,
        ...(entry === undefined ? {} : { detail: entry.name }),
        search: block,
        onPick: () => {
          pick({ block });
        },
      },
    ];
  });
  const plugins = [...drawn].flatMap(([plugin, name]) =>
    places(view.grid, { plugin })
      ? []
      : [
          {
            key: `plugin:${plugin}`,
            label: name,
            search: plugin,
            onPick: () => {
              pick({ plugin });
            },
          },
        ],
  );
  return [
    { key: "blocks", rows: blocks },
    { key: "plugins", title: "Plugins", rows: plugins },
  ];
}

// What the open panel picker shows, which follows the board.
function PanelMenu({ picking, onClose }: { readonly picking: Picking; readonly onClose: () => void }): ReactElement {
  const { core, drawn } = useDock();
  const board = useBoard(core);
  const { near } = picking;

  const groups = useMemo(() => {
    const pick = (panel: Panel) => {
      const view = dockState(core.store.board())?.active ?? null;
      if (view === null) return;
      const args: Json = near === undefined ? { view, panel } : { view, panel, near, side: "within" };
      core.commands.run("blockr.dock-add_panel", args).catch(() => {
        // Core tells the user why the board refused the command.
      });
    };
    return pickerGroups(board, core.blockRegistry(), drawn, pick);
  }, [board, core, drawn, near]);

  return (
    <Picker
      button={picking.button}
      align="start"
      label="Panels"
      caption="Show in this view"
      search={{ label: "Search", empty: "No panel matches." }}
      groups={groups}
      note={nothingLeft(board, drawn)}
      onClose={onClose}
    />
  );
}

// Why the panel picker has nothing to list, as today's suggest_panels_to_add()
// says it, at 655e323.
function nothingLeft(board: Board, drawn: ReadonlyMap<PluginClassId, string>): string {
  return Object.keys(board.blocks).length === 0 && drawn.size === 0
    ? "The board has no blocks yet. Add a new block to get started."
    : "All blocks and plugins are already in this view. Add a new block to the board first.";
}
