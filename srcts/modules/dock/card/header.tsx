// The header of a block's card, in one row, as the design system's "Dock
// header" has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#the-header):
// 1. The block's mark, whose tooltip is the name of the block's class, with
//    the package of its registry id as a badge.
//    The status dot sits on the mark's upper right corner, filled or a ring as
//    ./status-dot.ts gives it, with the status that it reads as its tooltip.
// 2. The block's name, which wraps to a second line and is cut after it.
//    A double-click on it, or Rename in the menu, turns it into a field that
//    holds the name, selected.
//    Enter or a click elsewhere runs blockr.core-rename_block with the new
//    name, Escape keeps the name, and an empty name, or one of spaces alone,
//    is refused on Enter and dropped on a click elsewhere.
// 3. The Controls toggle, once the block's UI has drawn anything, the
//    Preview toggle and the padlock, each a bare 26px tool with its name as
//    its tooltip and label.
//    A toggle runs blockr.dock-set_sections with the block's new set of
//    sections, and is pressed while its section shows.
//    The padlock shows whether the board lock leaves the block open to
//    readers, from the block's `locked`, closed for true or null and open for
//    false, and is pressed while the block is open.
//    A click on a closed padlock runs blockr.core-set_block_lock with false,
//    and one on an open padlock runs it with null.
// 4. The "…" button, which opens the block's menu on a press, or on ArrowDown
//    while it has the keyboard's focus.
//    The button and the menu are core's React SDK's, its MenuTrigger, Button,
//    Menu, MenuItem and MenuSection, and React Aria draws the menu in a portal
//    at the end of the page's body, below the button and lined up with its
//    right end.
//    The menu holds Rename, Append block, Copy block ID, with the id as meta
//    text, and, after a divider, Remove block, with the bin icon.
//    React Aria closes the menu as the user picks an item, on Escape and on a
//    press elsewhere, and gives the focus back to the button.
//    Rename and Append block run once the menu has closed, since they move the
//    focus out of the menu, which React Aria keeps from the rest of the page
//    until then.
//    Copy block ID copies within the click that picks it, which the
//    document's copy command needs (./clipboard.ts).
//    Append block opens the dock's "+" menu (../menu.tsx, ./classes.tsx) from
//    the same button, which stays the dock's own until core's React SDK draws
//    it (https://github.com/BristolMyersSquibb/blockr.dock/issues/630).
//    A press on the button while the "+" menu is open closes that menu alone.
// Under the board lock, the header has no toggle and no padlock, its name
// starts no rename, and its menu holds Copy block ID alone, since no command
// changes the rest there.

import type { Block, BlockId, RegistryEntry } from "@blockr/core";
import { Button, Menu, MenuItem, MenuSection, MenuTrigger, Text } from "@blockr/core/react";
import { useCallback, useEffect, useId, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { SECTIONS } from "../../../commands/index.ts";
import type { Section } from "../../../state/index.ts";
import { withAttributes } from "../attributes.tsx";
import { runEdit, useDock } from "../context.ts";
import { Menu as DockMenu } from "../menu.tsx";
import type { Closing } from "../menu.tsx";
import { classGroups } from "./classes.tsx";
import { copyText } from "./clipboard.ts";
import { BinIcon, DotsIcon, EyeIcon, LockIcon, SlidersIcon } from "./icons.tsx";
import { Mark, packageOf } from "./mark.tsx";
import type { StatusDot } from "./status-dot.ts";
import { useTooltip } from "./tooltip.tsx";

const GESTURE = "Double-click to edit";

export function Header({
  id,
  block,
  entry,
  sections,
  drawn,
  dot,
}: {
  id: BlockId;
  block: Block;
  entry: RegistryEntry | undefined;
  // The sections that the card shows.
  sections: readonly Section[];
  // Whether the block's UI has drawn anything.
  drawn: boolean;
  // The block's status dot, or none while the scheduler holds no status of
  // the block.
  dot: StatusDot | null;
}): ReactElement {
  const { core } = useDock();
  const locked = core.lock.board;
  const [renaming, setRenaming] = useState(false);
  // The "…" button, once it is drawn.
  const [menuButton, setMenuButton] = useState<HTMLButtonElement | null>(null);
  const open = block.locked === false;

  const toggle = (section: Section) => {
    const shown = sections.includes(section) ? sections.filter((each) => each !== section) : [...sections, section];
    runEdit(core, "blockr.dock-set_sections", {
      block: id,
      sections: SECTIONS.filter((each) => shown.includes(each)),
    });
  };
  const rename = () => {
    setRenaming(true);
  };

  return (
    <header className="blockr-card-header">
      <HeaderMark entry={entry} block={block} dot={dot} />
      <div className="blockr-card-title">
        {renaming ? (
          <NameField
            name={block.name}
            onCommit={(name) => {
              runEdit(core, "blockr.core-rename_block", { id, name });
            }}
            onDone={(restore) => {
              setRenaming(false);
              if (restore) menuButton?.focus({ preventScroll: true });
            }}
          />
        ) : (
          <Name name={block.name} editable={!locked} onRename={rename} />
        )}
      </div>
      <div className="blockr-card-tools">
        {locked ? null : (
          <>
            {drawn ? (
              <Tool
                label="Controls"
                pressed={sections.includes("inputs")}
                onClick={() => {
                  toggle("inputs");
                }}
              >
                <SlidersIcon />
              </Tool>
            ) : null}
            <Tool
              label="Preview"
              pressed={sections.includes("outputs")}
              onClick={() => {
                toggle("outputs");
              }}
            >
              <EyeIcon />
            </Tool>
            <Tool
              label={open ? "Open to readers" : "Locked for readers"}
              pressed={open}
              onClick={() => {
                runEdit(core, "blockr.core-set_block_lock", { id, locked: open ? null : false });
              }}
            >
              <LockIcon open={open} />
            </Tool>
          </>
        )}
        <MenuButton button={menuButton} onButton={setMenuButton} id={id} block={block} onRename={rename} />
      </div>
    </header>
  );
}

function HeaderMark({
  entry,
  block,
  dot,
}: {
  entry: RegistryEntry | undefined;
  block: Block;
  dot: StatusDot | null;
}): ReactElement {
  const pkg = packageOf(block.registry_id);
  const name = entry?.name ?? block.registry_id;
  const { props, tooltip } = useTooltip([pkg === null ? { text: name } : { text: name, badge: pkg }]);
  return (
    <span className="blockr-card-mark-frame">
      <Mark entry={entry} size={32} role="img" aria-label={pkg === null ? name : `${name}, ${pkg}`} {...props} />
      {tooltip}
      {dot === null ? null : <Dot dot={dot} />}
    </span>
  );
}

// The status dot, whose tooltip names the status that it reads, which a
// screen reader reads as the dot's name.
function Dot({ dot }: { dot: StatusDot }): ReactElement {
  const { props, tooltip } = useTooltip(dot.tooltip);
  return (
    <>
      <span
        className="blockr-card-dot"
        role="img"
        aria-label={dot.tooltip}
        data-status={dot.status}
        data-ring={dot.ring ? "" : undefined}
        {...props}
      />
      {tooltip}
    </>
  );
}

// Whether an element cuts off what it holds.
function cutOff(el: HTMLElement): boolean {
  return el.scrollHeight > el.clientHeight + 1 || el.scrollWidth > el.clientWidth + 1;
}

// The name, whose tooltip names the gesture that renames it, after the whole
// name where the header cuts it off, as the design system's "Renaming in
// place" has it.
function Name({ name, editable, onRename }: { name: string; editable: boolean; onRename: () => void }): ReactElement {
  const { props, tooltip } = useTooltip((el) => {
    const cut = cutOff(el);
    if (!editable) return cut ? name : null;
    return cut ? [{ text: name }, { text: GESTURE, muted: true }] : GESTURE;
  });
  return (
    <>
      <h3 className="blockr-card-name" data-editable={editable ? "" : undefined} onDoubleClick={editable ? onRename : undefined} {...props}>
        {name}
      </h3>
      {tooltip}
    </>
  );
}

function NameField({
  name,
  onCommit,
  onDone,
}: {
  name: string;
  onCommit: (name: string) => void;
  // Ends the rename, and gives the focus back to the "…" button where the
  // keyboard ended it.
  onDone: (restore: boolean) => void;
}): ReactElement {
  const [value, setValue] = useState(name);
  const [refused, setRefused] = useState(false);
  const field = useRef<HTMLInputElement>(null);
  const ended = useRef(false);
  const error = useId();

  useLayoutEffect(() => {
    field.current?.focus({ preventScroll: true });
    field.current?.select();
  }, []);

  const end = (commit: boolean, restore: boolean) => {
    if (ended.current) return;
    ended.current = true;
    const given = value.trim();
    if (commit && given !== name) onCommit(given);
    onDone(restore);
  };

  return (
    <div className="blockr-card-name-edit">
      <input
        ref={field}
        className="blockr-card-name-field"
        aria-label="Block name"
        aria-invalid={refused}
        aria-describedby={refused ? error : undefined}
        value={value}
        onChange={(event) => {
          setValue(event.target.value);
          if (event.target.value.trim() !== "") setRefused(false);
        }}
        onKeyDown={(event) => {
          if (event.key === "Enter") {
            event.preventDefault();
            if (value.trim() === "") setRefused(true);
            else end(true, true);
          } else if (event.key === "Escape") {
            event.preventDefault();
            event.stopPropagation();
            end(false, true);
          }
        }}
        onBlur={() => {
          end(value.trim() !== "", false);
        }}
      />
      {refused ? (
        <div id={error} className="blockr-card-name-error">
          A block needs a name
        </div>
      ) : null}
    </div>
  );
}

function Tool({
  label,
  pressed,
  onClick,
  children,
}: {
  label: string;
  pressed: boolean;
  onClick: () => void;
  children: ReactNode;
}): ReactElement {
  const { props, tooltip } = useTooltip(label);
  return (
    <>
      <button type="button" className="blockr-card-tool" aria-label={label} aria-pressed={pressed} onClick={onClick} {...props}>
        {children}
      </button>
      {tooltip}
    </>
  );
}

// The "…" button and the block's menu, as the comment at the top of this file
// says.
function MenuButton({
  button,
  onButton,
  id,
  block,
  onRename,
}: {
  // The button, once it is drawn, which `onButton` takes.
  button: HTMLButtonElement | null;
  onButton: (el: HTMLButtonElement | null) => void;
  id: BlockId;
  block: Block;
  onRename: () => void;
}): ReactElement {
  const { core, plugin } = useDock();
  const [open, setOpen] = useState(false);
  // The button from which the "+" menu is open, or null while it is closed.
  const [plus, setPlus] = useState<HTMLElement | null>(null);
  const afterClose = useAfterClose(open);
  const { props: tip, tooltip } = useTooltip(open || plus !== null ? null : "More actions");
  const render = useMemo(() => withAttributes({ ref: onButton }), [onButton]);

  const closePlus = useCallback(
    (how: Closing) => {
      if (how.restore) button?.focus({ preventScroll: true, focusVisible: !how.pointer });
      setPlus(null);
    },
    [button],
  );

  const copy = () => {
    void copyText(button?.ownerDocument ?? document, id).then((copied) => {
      if (!copied) {
        core.notifications.notify(plugin, {
          severity: "error",
          code: "copy_failed",
          message: `The browser did not let the dock copy the block ID "${id}".`,
        });
      }
    });
  };

  // Appends a block of the class to this block in one step, with a batch of
  // two commands: blockr.core-add_block adds the block, and
  // blockr.core-add_link links this block to it.
  // A batch hands no value from one command to the next, so the menu mints the
  // new block's id first, for the link to name.
  // The link lands on the class's first input where the class has several, of
  // which add_link picks none.
  const append = (entry: RegistryEntry) => {
    const [first] = entry.inputs;
    const input: Readonly<Record<string, string>> =
      entry.inputs.length > 1 && !entry.inputs.includes("...") && first !== undefined ? { input: first } : {};
    const { blocks } = core.store.board();
    const added = core.mintId((candidate) => Object.hasOwn(blocks, candidate));
    runEdit(core, [
      { id: "blockr.core-add_block", args: { registry_id: entry.id, id: added } },
      { id: "blockr.core-add_link", args: { from: id, to: added, ...input } },
    ]);
  };

  const copyItem = (
    <MenuItem key="copy" id="copy" textValue="Copy block ID" onAction={copy}>
      <Text slot="label">Copy block ID</Text>
      <Text slot="description" className="blockr-card-menu-meta">
        {id}
      </Text>
    </MenuItem>
  );
  // Under the board lock, the menu holds Copy block ID alone.
  const sections = core.lock.board
    ? [
        <MenuSection key="actions" id="actions">
          {copyItem}
        </MenuSection>,
      ]
    : [
        <MenuSection key="actions" id="actions">
          <MenuItem
            id="rename"
            onAction={() => {
              afterClose(onRename);
            }}
          >
            Rename
          </MenuItem>
          <MenuItem
            id="append"
            onAction={() => {
              afterClose(() => {
                setPlus(button);
              });
            }}
          >
            Append block
          </MenuItem>
          {copyItem}
        </MenuSection>,
        <MenuSection key="last" id="last" className="blockr-card-menu-divided">
          <MenuItem
            id="remove"
            textValue="Remove block"
            className="blockr-card-menu-danger"
            onAction={() => {
              runEdit(core, "blockr.core-remove_block", { id });
            }}
          >
            <Text slot="label" className="blockr-card-menu-lead">
              <BinIcon />
              Remove block
            </Text>
          </MenuItem>
        </MenuSection>,
      ];

  return (
    <>
      <MenuTrigger
        placement="bottom end"
        isOpen={open}
        onOpenChange={(next) => {
          // A press on the button while the "+" menu is open closes that
          // menu alone, which closes on no press on its button itself.
          if (next && plus !== null) {
            setPlus(null);
            return;
          }
          setOpen(next);
        }}
      >
        <Button
          variant="quiet"
          className="blockr-card-tool"
          aria-label="More actions"
          // The button says it is expanded while the "+" menu that it opened
          // is open too, as React Aria says it while the block's menu is.
          aria-expanded={plus === null ? undefined : true}
          render={render}
          {...tip}
          onFocus={(event) => {
            // React Aria's focus event names the button as an Element.
            if (event.currentTarget instanceof HTMLElement) tip.onFocus({ currentTarget: event.currentTarget });
          }}
        >
          <DotsIcon />
        </Button>
        <Menu aria-label="More actions" className="blockr-card-menu">
          {sections}
        </Menu>
      </MenuTrigger>
      {tooltip}
      {plus === null ? null : (
        <DockMenu
          anchor={plus}
          label="Block classes"
          align="end"
          caption={`Append to ${block.name}`}
          search={{ label: "Search blocks", empty: "No matches" }}
          groups={classGroups(
            Object.values(core.blockRegistry()).filter((entry) => entry.inputs.length > 0),
            append,
          )}
          keyboard={false}
          onClose={closePlus}
        />
      )}
    </>
  );
}

// Runs what the user picked in the block's menu once the menu has closed, as
// Rename and Append block, which move the focus out of the menu.
// Until the menu has closed, React Aria keeps the rest of the page inert, so
// that no element there takes the focus.
// React Aria frees the page in the cleanup of an effect as the menu goes, and
// React runs every cleanup before it runs the effect here.
function useAfterClose(open: boolean): (run: () => void) => void {
  const [queued, setQueued] = useState<(() => void) | null>(null);
  useEffect(() => {
    if (queued === null || open) return;
    queued();
    // eslint-disable-next-line react-hooks/set-state-in-effect -- What the user picked waits for React Aria to free the page, which it does in an effect.
    setQueued(null);
  }, [queued, open]);
  return useCallback((run: () => void) => {
    setQueued(() => run);
  }, []);
}
