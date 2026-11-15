// The header of a block's card, in one row, as the design system's "Dock
// header" has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#the-header):
// 1. The block's mark, whose tooltip is the name of the block's class, with
//    the package of its registry id as a badge.
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
// 4. The "…" button, which opens the block's menu on a click, or on ArrowDown
//    while it has the keyboard's focus (./menu.tsx): Rename, Append block,
//    which opens the dock's "+" menu from the button (./picker.tsx), Copy
//    block ID and, after a divider, Remove block.
// Under the board lock, the header has no toggle and no padlock, its name
// starts no rename, and its menu holds Copy block ID alone, since no command
// changes the rest there.

import type { Block, BlockId, RegistryEntry } from "@blockr/core";
import { useCallback, useId, useLayoutEffect, useRef, useState } from "react";
import type { ReactElement, ReactNode, RefObject } from "react";
import { SECTIONS } from "../../../commands/index.ts";
import type { Section } from "../../../state/index.ts";
import { copyText } from "./clipboard.ts";
import { BinIcon, DotsIcon, EyeIcon, LockIcon, SlidersIcon } from "./icons.tsx";
import { Mark, packageOf } from "./mark.tsx";
import { ActionMenu } from "./menu.tsx";
import type { Closing, MenuRow } from "./menu.tsx";
import { runEdit, useDock } from "../context.ts";
import { BlockPicker } from "./picker.tsx";
import { useTooltip } from "./tooltip.tsx";

const GESTURE = "Double-click to edit";

export function Header({
  id,
  block,
  entry,
  sections,
  drawn,
}: {
  id: BlockId;
  block: Block;
  entry: RegistryEntry | undefined;
  // The sections that the card shows.
  sections: readonly Section[];
  // Whether the block's UI has drawn anything.
  drawn: boolean;
}): ReactElement {
  const { core, plugin } = useDock();
  const locked = core.lock.board;
  const [renaming, setRenaming] = useState(false);
  const menuButton = useRef<HTMLButtonElement>(null);
  const open = block.locked === false;

  const toggle = (section: Section) => {
    const shown = sections.includes(section) ? sections.filter((each) => each !== section) : [...sections, section];
    runEdit(core, plugin, "blockr.dock-set_sections", {
      block: id,
      sections: SECTIONS.filter((each) => shown.includes(each)),
    });
  };
  const rename = () => {
    setRenaming(true);
  };

  return (
    <header className="blockr-card-header">
      <HeaderMark entry={entry} block={block} />
      <div className="blockr-card-title">
        {renaming ? (
          <NameField
            name={block.name}
            onCommit={(name) => {
              runEdit(core, plugin, "blockr.core-rename_block", { id, name });
            }}
            onDone={(restore) => {
              setRenaming(false);
              if (restore) menuButton.current?.focus({ preventScroll: true });
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
                runEdit(core, plugin, "blockr.core-set_block_lock", { id, locked: open ? null : false });
              }}
            >
              <LockIcon open={open} />
            </Tool>
          </>
        )}
        <MenuButton ref={menuButton} id={id} block={block} onRename={rename} />
      </div>
    </header>
  );
}

function HeaderMark({ entry, block }: { entry: RegistryEntry | undefined; block: Block }): ReactElement {
  const pkg = packageOf(block.registry_id);
  const name = entry?.name ?? block.registry_id;
  const { props, tooltip } = useTooltip([pkg === null ? { text: name } : { text: name, badge: pkg }]);
  return (
    <>
      <Mark entry={entry} size={32} role="img" aria-label={pkg === null ? name : `${name}, ${pkg}`} {...props} />
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

// Which menu of the "…" button is open, with the button it opens from.
type Open =
  | { readonly menu: "actions"; readonly anchor: HTMLElement; readonly keyboard: boolean }
  | { readonly menu: "append"; readonly anchor: HTMLElement };

function MenuButton({
  ref,
  id,
  block,
  onRename,
}: {
  ref: RefObject<HTMLButtonElement | null>;
  id: BlockId;
  block: Block;
  onRename: () => void;
}): ReactElement {
  const { core, plugin } = useDock();
  const [open, setOpen] = useState<Open | null>(null);
  const { props, tooltip } = useTooltip(open === null ? "More actions" : null);

  const close = useCallback(
    (how: Closing) => {
      setOpen(null);
      if (how.restore) ref.current?.focus({ preventScroll: true, focusVisible: !how.pointer });
    },
    [ref],
  );

  const rows = (anchor: HTMLElement): MenuRow[] => {
    const copy: MenuRow = {
      key: "copy",
      label: "Copy block ID",
      meta: id,
      onSelect() {
        void copyText(anchor.ownerDocument, id).then((copied) => {
          if (!copied) {
            core.notifications.notify(plugin, {
              severity: "error",
              code: "copy_failed",
              message: `The browser did not let the dock copy the block ID "${id}".`,
            });
          }
        });
      },
    };
    if (core.lock.board) return [copy];
    return [
      { key: "rename", label: "Rename", onSelect: onRename },
      {
        key: "append",
        label: "Append block",
        onSelect() {
          setOpen({ menu: "append", anchor });
        },
      },
      copy,
      {
        key: "remove",
        label: "Remove block",
        icon: <BinIcon />,
        danger: true,
        divided: true,
        onSelect() {
          runEdit(core, plugin, "blockr.core-remove_block", { id });
        },
      },
    ];
  };

  // Appends a block of the class, whose link from this block lands on the
  // class's first input where the class has several, of which core picks none.
  const append = (entry: RegistryEntry) => {
    const [first] = entry.inputs;
    const input: Readonly<Record<string, string>> =
      entry.inputs.length > 1 && !entry.inputs.includes("...") && first !== undefined ? { input: first } : {};
    runEdit(core, plugin, "blockr.core-append_block", { from: id, registry_id: entry.id, ...input });
  };

  return (
    <>
      <button
        ref={ref}
        type="button"
        className="blockr-card-tool"
        aria-label="More actions"
        aria-haspopup="menu"
        aria-expanded={open !== null}
        onClick={(event) => {
          setOpen(open === null ? { menu: "actions", anchor: event.currentTarget, keyboard: event.detail === 0 } : null);
        }}
        onKeyDown={(event) => {
          if (event.key !== "ArrowDown") return;
          event.preventDefault();
          setOpen({ menu: "actions", anchor: event.currentTarget, keyboard: true });
        }}
        {...props}
      >
        <DotsIcon />
      </button>
      {tooltip}
      {open?.menu === "actions" ? (
        <ActionMenu anchor={open.anchor} label="More actions" rows={rows(open.anchor)} keyboard={open.keyboard} onClose={close} />
      ) : null}
      {open?.menu === "append" ? (
        <BlockPicker
          anchor={open.anchor}
          caption={`Append to ${block.name}`}
          entries={Object.values(core.blockRegistry()).filter((entry) => entry.inputs.length > 0)}
          onPick={append}
          onClose={close}
        />
      ) : null}
    </>
  );
}
