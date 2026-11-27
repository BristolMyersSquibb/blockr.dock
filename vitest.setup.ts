// The setup of every test file (./vitest.config.ts).
//
// A test fails where it prints an error or a warning to the console, through
// console.error or console.warn, which Vitest shows for a passing test only on
// request, so that the message would go unseen.
// A test that sets off an error on purpose takes over console.error with a spy
// of its own (./srcts/modules/dock/testing.ts), which then sees the message in
// place of the setup.
// The messages include React's warnings of an update outside act(): the stubs
// of blockr.ui, which core's stubs load, turn on React's act environment, in
// which React expects each update that a test sets off to run inside act(), so
// that the page has drawn it before the test reads the page.
// One message stays allowed, until core no longer sets it off: React's error
// that core's drawForm() calls flushSync() from inside a lifecycle method, as
// core's Mounted component mounts the forms of the Edit board plugin in a
// layout effect (https://github.com/BristolMyersSquibb/blockr.core/issues/900).
//
// The setup also gives happy-dom the element search, which happy-dom does not
// know (https://github.com/capricorn86/happy-dom/issues/2640), and which
// browsers draw as a plain HTMLElement.
// React would warn that the tag is unrecognized where core's Edit board plugin
// draws one.
// The entry is that of section, which a browser draws alike.
import { format } from "node:util";
import HTMLElementConfig from "happy-dom/lib/config/HTMLElementConfig.js";
import { afterEach, beforeEach, expect } from "vitest";

const config: Record<string, unknown> = HTMLElementConfig;
config.search ??= HTMLElementConfig.section;

let printed: string[] = [];

// Whether the message is core's error that blockr.core takes up, as above.
const allowed = (message: string) =>
  message.startsWith("flushSync was called from inside a lifecycle method") && /\bdrawForm\b/.test(new Error().stack ?? "");

for (const level of ["error", "warn"] as const) {
  const original = console[level].bind(console);
  console[level] = (...args: unknown[]) => {
    const message = format(...args);
    if (!allowed(message)) printed.push(`console.${level}: ${message.split("\n")[0] ?? message}`);
    original(...args);
  };
}

beforeEach(() => {
  printed = [];
});

afterEach(() => {
  expect(printed, "The test printed what it did not set off on purpose").toEqual([]);
});
