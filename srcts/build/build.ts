// Builds the dock's client into inst/js, which the R package ships and git
// keeps, since R CMD check and CRAN run no Node. Run: npm run build
//
// - A module, srcts/modules/<name>/index.ts or index.tsx, is bundled into
//   <name>.js, as the module of the dock plugin's class is into dock.js.
// - Each takes the build preset that blockr.core ships as `@blockr/core/build`,
//   which leaves its imports of React and of core's React SDK, its binding and
//   its controls imports, which the page that R serves resolves through its
//   import map. So no bundle holds any of them, while a bundle holds the other
//   packages that its module imports, as dock.js holds dockview.
// - An index.css beside a module is its stylesheet, bundled into <name>.css
//   with the stylesheets it imports, as dock.css holds dockview's.
// - Both are minified, as core minifies the React it ships, since most of
//   each is dockview's. The license comments of what a bundle holds stay, at
//   its end.
// - The JSON Schema of each of the dock's board options, in
//   srcts/options/schemas.ts, is written beside the board option's module, as
//   compact-option.schema.json beside compact-option.js, as core's build
//   writes those of its own. R writes the value of a board option that an app
//   script gives by it.
//
// The build owns inst/js and empties it first, so that nothing is left of a
// module that is gone.

import { existsSync } from "node:fs";
import { mkdir, readdir, rm, writeFile } from "node:fs/promises";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
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

// The modules, each by the name of the file that it is bundled into and its
// directory.
async function modules(src: string): Promise<Entry[]> {
  const dir = join(src, "modules");
  const dirs = existsSync(dir) ? await readdir(dir, { withFileTypes: true }) : [];
  return dirs
    .filter((entry) => entry.isDirectory())
    .map((entry) => ({ name: entry.name, dir: join(dir, entry.name) }));
}

// Builds the client of the package at `root` and gives the files it wrote,
// relative to `root`.
export async function build(root: string): Promise<string[]> {
  const out = join(root, "inst", "js");
  const entries = await modules(join(root, "srcts"));

  await rm(out, { recursive: true, force: true });
  await mkdir(out, { recursive: true });

  const options = {
    absWorkingDir: root,
    outdir: out,
    minify: true,
    logLevel: "warning",
    metafile: true,
  } satisfies esbuild.BuildOptions;

  const stylesheets = entries
    .map((entry) => ({ in: join(entry.dir, "index.css"), out: entry.name }))
    .filter((entry) => existsSync(entry.in));

  const results = [
    ...(entries.length === 0
      ? []
      : [
          await esbuild.build({
            ...preset,
            ...options,
            entryPoints: entries.map((entry) => ({ in: entryFile(entry.dir), out: entry.name })),
          }),
        ]),
    ...(stylesheets.length === 0
      ? []
      : [await esbuild.build({ ...options, bundle: true, entryPoints: stylesheets })]),
  ];

  if (results.some((result) => result.warnings.length > 0)) {
    throw new Error("esbuild warned, as printed above");
  }

  return [
    ...results.flatMap((result) => Object.keys(result.metafile.outputs)),
    ...(await optionSchemas(root, out)),
  ].sort();
}

// Writes the JSON Schema of each of the dock's board options into `out`,
// beside the board option's module, and gives the files it wrote, relative to
// `root`. It reads them from srcts/options/schemas.ts alone, which Node runs
// as it is, where the index of srcts/options brings in the JSX of the board
// options' UIs, which Node cannot run.
async function optionSchemas(root: string, out: string): Promise<string[]> {
  const source = join(root, "srcts", "options", "schemas.ts");
  if (!existsSync(source)) return [];

  const { optionSchemas: schemas } = (await import(pathToFileURL(source).href)) as {
    optionSchemas: Readonly<Record<string, unknown>>;
  };

  const written: string[] = [];

  for (const [id, schema] of Object.entries(schemas)) {
    const file = join(out, `${optionModuleName(id)}.schema.json`);
    await writeFile(file, `${JSON.stringify(schema, null, 2)}\n`);
    written.push(relative(root, file));
  }

  return written;
}

// The name of the module of the dock's board option with the id given, as
// compact-option for compact, which blockr.dock lists in R with the board
// option.
function optionModuleName(id: string): string {
  return `${id.replaceAll("_", "-")}-option`;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  for (const file of await build(resolve(dirname(fileURLToPath(import.meta.url)), "../.."))) {
    console.log(file);
  }
}
