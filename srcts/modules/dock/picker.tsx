// The panel picker, which lists what the active view does not show, and adds
// the panel that the user picks to the view
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#a-plugins-panel).
// It lists the board's blocks that the view does not show, each by the mark of
// its class, its name and the name of its class, in the order of the board,
// as the design system lists the blocks of a board in a menu.
// After them, under "Plugins", it lists the app's plugins with a `ui` that the
// view does not show, by their names, in the order of the app's plugins.
// A search at its top filters them by name, by the name of a block's class
// and by id, so that the picker serves a board of many blocks.
// Today's Blockr.menu shows one only where it lists more than eight, at
// 655e323.
// Where the picker has nothing to list, it says why in place of the list.
//
// Two buttons open it: the "+" in the header of each tab group, and the "Add
// panel" button of an empty view.
// A pick runs blockr.dock-add_panel on the active view.
// From a tab group's "+", the pick goes near the panel that the group shows,
// so the panel becomes a new tab in that group.
// From an empty view, the panel goes to the default place.
// Where the board refuses the command, core tells the user why, as an error of
// the code board_edit_failed.
//
// The picker is one of the dock's menus, which it draws and places, and whose
// keys it takes (./menu.tsx).

import type { BlockRegistry, Board, Json, PluginClassId } from "@blockr/core";
import { createContext, useCallback, useContext, useId, useMemo, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { activeView, stateOf } from "../../commands/index.ts";
import { places } from "../../state/index.ts";
import type { Panel } from "../../state/index.ts";
import { Mark } from "./card/index.ts";
import { useBoard, useDock } from "./context.ts";
import { Menu, useMenuButton } from "./menu.tsx";
import type { Closing, MenuGroup } from "./menu.tsx";

// The open picker.
interface Picking {
  // The button that opened it, with an id of the button's own.
  readonly id: string;
  readonly button: HTMLElement;
  // The panel near which a pick goes, the one that the group of a tab group's
  // "+" shows, or undefined for the default place.
  readonly near: Panel | undefined;
  // Whether the keyboard opened it.
  readonly keyboard: boolean;
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

// The picker, which the buttons among its children open.
export function PanelPicker({ children }: { readonly children: ReactNode }): ReactElement {
  const [picking, setPicking] = useState<Picking | undefined>(undefined);
  const state = useMemo(() => ({ picking, setPicking }), [picking]);
  const close = useCallback(
    (how: Closing) => {
      setPicking(undefined);
      if (how.restore) picking?.button.focus({ preventScroll: true, focusVisible: !how.pointer });
    },
    [picking],
  );
  return (
    <PickerContext value={state}>
      {children}
      {picking === undefined ? null : <PickerMenu picking={picking} onClose={close} />}
    </PickerContext>
  );
}

// The props of a button that opens the picker.
// The function `near` gives the panel near which a pick goes, or undefined for
// the default place, as the user opens the picker.
export function usePickerButton(near: () => Panel | undefined) {
  const { picking, setPicking } = usePicker();
  const id = useId();
  return useMenuButton(
    picking?.id === id,
    (button, keyboard) => {
      setPicking({ id, button, near: near(), keyboard });
    },
    () => {
      setPicking(undefined);
    },
  );
}

// The rows of the picker on a board, where the dock draws the panels of the
// plugins given: those that the active view does not show, none where there
// is no active view.
function pickerGroups(
  board: Board,
  registry: BlockRegistry,
  drawn: ReadonlyMap<PluginClassId, string>,
  pick: (panel: Panel) => void,
): MenuGroup[] {
  const view = activeView(board);
  if (view === undefined) return [];
  const blocks = Object.entries(board.blocks).flatMap(([block, held]) => {
    if (places(view.grid, { block })) return [];
    const entry = registry[held.registry_id];
    return [
      {
        key: block,
        label: held.name,
        lead: <Mark entry={entry} size={24} />,
        ...(entry === undefined ? {} : { detail: entry.name }),
        search: block,
        onSelect: () => {
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
            key: plugin,
            label: name,
            search: plugin,
            onSelect: () => {
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

// What the open picker shows, which follows the board.
function PickerMenu({ picking, onClose }: { readonly picking: Picking; readonly onClose: (how: Closing) => void }): ReactElement {
  const { core, drawn } = useDock();
  const board = useBoard(core);
  const { near } = picking;

  const groups = useMemo(() => {
    const pick = (panel: Panel) => {
      const view = stateOf(core.store.board())?.active ?? null;
      if (view === null) return;
      const args: Json = near === undefined ? { view, panel } : { view, panel, near, side: "within" };
      core.commands.run("blockr.dock-add_panel", args).catch(() => {
        // Core tells the user why the board refused the command.
      });
    };
    return pickerGroups(board, core.blockRegistry(), drawn, pick);
  }, [board, core, drawn, near]);

  return (
    <Menu
      anchor={picking.button}
      label="Panels"
      caption="Show in this view"
      search={{ label: "Search", empty: "No panel matches." }}
      groups={groups}
      note={nothingLeft(board, drawn)}
      keyboard={picking.keyboard}
      onClose={onClose}
    />
  );
}

// Why the picker has nothing to list, as today's suggest_panels_to_add() says
// it, at 655e323.
function nothingLeft(board: Board, drawn: ReadonlyMap<PluginClassId, string>): string {
  return Object.keys(board.blocks).length === 0 && drawn.size === 0
    ? "The board has no blocks yet. Add a new block to get started."
    : "All blocks and plugins are already in this view. Add a new block to the board first.";
}
