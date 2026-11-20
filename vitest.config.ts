import { defineConfig } from "vitest/config";

// The tests run on happy-dom. They import blockr.core's CoreApi for tests
// from @blockr/core, which `npm test` installs first, and React from the
// package's own node_modules.
export default defineConfig({
  test: {
    environment: "happy-dom",
    include: ["srcts/**/*.test.{ts,tsx}"],
    // The typescript job of blockr.ci uploads coverage/lcov.info to Codecov.
    // The lcov reporter writes that file.
    // The other reporters are those that Vitest runs by default.
    // The coverage counts every source under srcts/, so that a file that no
    // test imports counts as uncovered.
    // It leaves out the tests and testing.ts, which starts the dock for them.
    coverage: {
      enabled: true,
      include: ["srcts/**/*.{ts,tsx}"],
      exclude: ["srcts/**/*.test.{ts,tsx}", "srcts/modules/dock/testing.ts"],
      reporter: ["text", "html", "clover", "json", "lcov"],
    },
  },
});
