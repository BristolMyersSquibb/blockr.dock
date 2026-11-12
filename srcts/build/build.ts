// Builds the dock's client into inst/js, which the R package ships and git
// keeps, since R CMD check and CRAN run no Node. Run: npm run build
//
// - The startup module, srcts/startup/index.ts, is bundled into startup.js.
// - A module, srcts/modules/<name>/index.ts or index.tsx, is bundled into
//   <name>.js, as the frontend's module is into dock.js.
// - Both take the build preset that blockr.core ships as `@blockr/core/build`,
//   which leaves their imports of React and of core's React SDK, its binding
//   and its controls imports, which the page that R serves resolves through
//   its import map. So no bundle holds any of them, while a bundle holds the
//   other packages that its module imports, as dock.js holds dockview.
// - An index.css beside a module is its stylesheet, bundled into <name>.css
//   with the stylesheets it imports, as dock.css holds dockview's.
//
// The build owns inst/js and empties it first, so that nothing is left of a
// module that is gone.

import { existsSync } from "node:fs";
import { mkdir, readdir, rm } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { preset } from "@blockr/core/build";
import * as esbuild from "esbuild";

interface Entry {
  readonly name: string;
  readonly dir: string;
}

// The file that a module's directory starts from, index.ts or, where it draws
// with React, index.tsx.
function entryFile(dir: string): string {
  const tsx = join(dir, "index.tsx");
  return existsSync(tsx) ? tsx : join(dir, "index.ts");
}

// The startup module and the modules, each by the name of the file that it is
// bundled into and its directory.
async function entries(src: string): Promise<{ startup: Entry; modules: Entry[] }> {
  const modules = join(src, "modules");
  const dirs = existsSync(modules) ? await readdir(modules, { withFileTypes: true }) : [];
  return {
    startup: { name: "startup", dir: join(src, "startup") },
    modules: dirs
      .filter((dir) => dir.isDirectory())
      .map((dir) => ({ name: dir.name, dir: join(modules, dir.name) })),
  };
}

// Builds the client of the package at `root` and gives the files it wrote,
// relative to `root`.
export async function build(root: string): Promise<string[]> {
  const out = join(root, "inst", "js");
  const { startup, modules } = await entries(join(root, "srcts"));

  await rm(out, { recursive: true, force: true });
  await mkdir(out, { recursive: true });

  const options = {
    absWorkingDir: root,
    outdir: out,
    logLevel: "warning",
    metafile: true,
  } satisfies esbuild.BuildOptions;

  const stylesheets = modules
    .map((entry) => ({ in: join(entry.dir, "index.css"), out: entry.name }))
    .filter((entry) => existsSync(entry.in));

  const results = [
    await esbuild.build({
      ...preset,
      ...options,
      entryPoints: [startup, ...modules].map((entry) => ({ in: entryFile(entry.dir), out: entry.name })),
    }),
    ...(stylesheets.length === 0
      ? []
      : [await esbuild.build({ ...options, bundle: true, entryPoints: stylesheets })]),
  ];

  if (results.some((result) => result.warnings.length > 0)) {
    throw new Error("esbuild warned, as printed above");
  }

  return results.flatMap((result) => Object.keys(result.metafile.outputs)).sort();
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  for (const file of await build(resolve(dirname(fileURLToPath(import.meta.url)), "../.."))) {
    console.log(file);
  }
}
