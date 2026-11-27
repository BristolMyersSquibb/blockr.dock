import { defineConfig } from "vitest/config";

// The tests run on happy-dom. They import blockr.core's CoreApi for tests
// from @blockr/core, which `npm test` installs first, and React from the
// package's own node_modules.
export default defineConfig({
  test: {
    environment: "happy-dom",
    include: ["srcts/**/*.test.{ts,tsx}"],
    // A test fails on an error or a warning that it prints, with the setup
    // that blockr.ui offers packages.
    setupFiles: ["@blockr/ui/stubs/setup"],
    // The typescript job of blockr.ci uploads coverage/lcov.info to Codecov.
    // The lcov reporter writes that file, and an HTML report beside it in
    // coverage/lcov-report.
    // Istanbul measures the coverage, as it does in blockr.core.
    // Istanbul counts the code of the sources in srcts alone.
    // V8 counts all the code that runs, that of node_modules included.
    // That makes the tests slower under V8.
    // The coverage counts every source under srcts/, so that a file that no
    // test imports counts as uncovered.
    // Vitest leaves out the test files by itself.
    // The `exclude` option leaves out testing.ts, which starts the dock for
    // the tests.
    coverage: {
      enabled: true,
      provider: "istanbul",
      reporter: ["text", "lcov"],
      include: ["srcts/**/*.{ts,tsx}"],
      exclude: ["srcts/modules/dock/testing.ts"],
    },
  },
});
