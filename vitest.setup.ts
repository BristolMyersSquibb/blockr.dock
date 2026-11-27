// The setup of every test file (./vitest.config.ts): a test fails where React
// warns of an update outside act().
//
// The stubs of blockr.ui, which core's stubs load, turn on React's act
// environment, in which React expects each update that a test sets off to run
// inside act(), so that the page has drawn it before the test reads the page.
// React warns of an update that does not through console.error, and Vitest
// prints what a passing test logs only on request, so the warning would go
// unseen.
// So the setup collects each such warning, and fails the test that set it off
// with the names of the components that updated.
import { afterEach, beforeEach, expect } from "vitest";

const error = console.error.bind(console);
let warned: string[] = [];

console.error = (...args: unknown[]) => {
  const [format, name] = args;
  if (typeof format === "string" && /\bact\(/.test(format)) {
    warned.push(typeof name === "string" ? name : (format.split("\n")[0] ?? format));
  }
  error(...args);
};

beforeEach(() => {
  warned = [];
});

afterEach(() => {
  const components = [...new Set(warned)];
  expect(components, `React warned of an update outside act() in ${components.join(", ")}`).toEqual([]);
});
