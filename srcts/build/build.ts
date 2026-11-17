// Builds the dock's client into inst/js, which the R package ships and git
// keeps, since R CMD check and CRAN run no Node. Run: npm run build
//
// - A module, srcts/modules/<name>/index.ts or index.tsx, is bundled into
//   <name>.js, as the module of the dock plugin's class is into dock.js.
// - Each takes the build preset that blockr.core ships as `@blockr/core/build`,
//   which leaves its imports of React and of core's React SDK, its binding and
//   its controls imports, which the page that R serves resolves through its
//   import map. So no bundle holds any of them.
// - The packages that the dock bundles from other projects are those that
//   package.json lists in `dependencies`: dockview-react and
//   fractional-indexing. The build bundles them, minified, into a file of
//   their own, vendor-<hash>.js, with every name that they export. The
//   modules import them from that file by its name. So a module's bundle
//   holds only the dock's own code, unminified, and a diff of inst/js shows
//   what an edit of srcts changed.
// - An index.css beside a module is its stylesheet, bundled, unminified, into
//   <name>.css. The build bundles the stylesheets of those packages that it
//   imports, as the dock's imports dockview's, minified, into a file of their
//   own, vendor-<hash>.css. The module's stylesheet imports that file in
//   their place.
// - R serves both vendor files beside the modules, with no hash in their
//   URLs. So the name of each holds a hash of its content, as blockr.core
//   names the file that holds React, and no browser runs an old copy from its
//   cache. The license comments of what each holds stay, at its end.
// - The builds of the modules and of their stylesheets take the vendor check
//   that blockr.core ships beside the preset. It fails the build where one of
//   those files would hold code from node_modules. A source that imported
//   dockview-core, rather than dockview-react, would fail it.
// - The JSON Schema of each of the dock's board options, in
//   srcts/options/schemas.ts, is written beside the board option's module, as
//   compact-option.schema.json beside compact-option.js, as core's build
//   writes those of its own. R writes the value of a board option that an app
//   script gives by it.
// - The build generates the JSON Schema of the dock plugin's state from the
//   type DockState in srcts/state/types.ts, as core's build generates the
//   schema of a block's state. It writes it beside the dock's module, as
//   dock.schema.json, where R finds it. The dock's module imports the file as
//   the schema of its class's state, so the build writes it before it
//   bundles.
// - What each file that the build writes imports up front goes into
//   preload.json, by staticImports() from `@blockr/core/build`, as core's
//   build writes its own.
//   R names those files in the head of the page that it serves, as the vendor
//   files that dock.js and dock.css import.
//   So the browser fetches them as the page loads, rather than each once the
//   file that imports it has loaded.
//   The vendor file of scripts imports React, so it has a list of its own.
//
// The build owns inst/js and empties it first, so that nothing is left of a
// module that is gone.

import { createHash } from "node:crypto";
import { existsSync } from "node:fs";
import { mkdir, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { preset, staticImports, vendorCheck } from "@blockr/core/build";
import * as esbuild from "esbuild";
import { createGenerator } from "ts-json-schema-generator";

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

// The packages that the build bundles into the vendor files, by their names:
// those that package.json lists in `dependencies`.
async function vendorPackages(root: string): Promise<string[]> {
  const manifest = JSON.parse(await readFile(join(root, "package.json"), "utf8")) as {
    readonly dependencies?: Readonly<Record<string, string>>;
  };
  return Object.keys(manifest.dependencies ?? {});
}

// Builds the client of the package at `root` and gives the files it wrote,
// relative to `root`.
export async function build(root: string): Promise<string[]> {
  const out = join(root, "inst", "js");
  const entries = await modules(join(root, "srcts"));
  const packages = await vendorPackages(root);

  await rm(out, { recursive: true, force: true });
  await mkdir(out, { recursive: true });
  const schemas = await stateSchema(root, out);

  const options = {
    absWorkingDir: root,
    outdir: out,
    logLevel: "warning",
    metafile: true,
  } satisfies esbuild.BuildOptions;

  const stylesheets = entries
    .map((entry) => ({ in: join(entry.dir, "index.css"), out: entry.name }))
    .filter((entry) => existsSync(entry.in));

  const vendorJs =
    entries.length === 0 || packages.length === 0
      ? undefined
      : await vendorBundle(root, out, "js", packages.map((name) => `export * from ${JSON.stringify(name)};`));
  const sheets = stylesheets.length === 0 ? [] : await vendorStylesheets(root, out, stylesheets, packages);
  const vendorCss =
    sheets.length === 0
      ? undefined
      : await vendorBundle(root, out, "css", sheets.map((path) => `@import ${JSON.stringify(path)};`));
  const vendors = [vendorJs, vendorCss].filter((vendor) => vendor !== undefined);

  const results = [
    ...(entries.length === 0
      ? []
      : [
          await esbuild.build({
            ...preset,
            ...options,
            plugins: [
              ...(vendorJs === undefined ? [] : [vendorImports(packages, vendorJs.file)]),
              ...preset.plugins,
              vendorCheck,
            ],
            entryPoints: entries.map((entry) => ({ in: entryFile(entry.dir), out: entry.name })),
          }),
        ]),
    ...(stylesheets.length === 0
      ? []
      : [
          await esbuild.build({
            ...options,
            bundle: true,
            plugins: [
              ...(vendorCss === undefined ? [] : [vendorStylesheetImports(packages, () => `./${vendorCss.file}`)]),
              vendorCheck,
            ],
            entryPoints: stylesheets,
          }),
        ]),
  ];

  if (results.some((result) => result.warnings.length > 0)) {
    throw new Error("esbuild warned, as printed above");
  }

  return [
    ...results.flatMap((result) => Object.keys(result.metafile.outputs)),
    ...vendors.map((vendor) => relative(root, join(out, vendor.file))),
    ...schemas,
    ...(await optionSchemas(root, out)),
    await preloadList(root, out, results.map((result) => result.metafile), vendors),
  ].sort();
}

// A vendor file that vendorBundle() wrote: its name, and what it imports up
// front, as staticImports() lists it.
interface Vendor {
  readonly file: string;
  readonly imports: readonly string[];
}

// Bundles what the lines of a script or a stylesheet import from the vendored
// packages, minified, into a vendor file in `out`. It gives the file's name and
// what the file imports up front, as the script imports React.
// The name holds a hash of the file's content, as vendor-3f2a9c1d.js. A script
// takes the build preset, so that dockview's imports of React stay imports.
async function vendorBundle(
  root: string,
  out: string,
  loader: "js" | "css",
  lines: readonly string[],
): Promise<Vendor> {
  const result = await esbuild.build({
    ...(loader === "js" ? preset : { bundle: true }),
    absWorkingDir: root,
    stdin: { contents: lines.join("\n"), resolveDir: root, loader },
    write: false,
    metafile: true,
    minify: true,
    logLevel: "warning",
  });
  if (result.warnings.length > 0) {
    throw new Error("esbuild warned, as printed above");
  }

  const text = result.outputFiles[0]?.text ?? "";
  const file = `vendor-${createHash("sha256").update(text).digest("hex").slice(0, 8)}.${loader}`;
  await writeFile(join(out, file), text);
  // The build has no path for its output, so its metafile names it after
  // stdin, as stdin.js.
  // The file's imports go under the file's name instead.
  return { file, imports: Object.values(staticImports([result.metafile])).flat() };
}

// Writes what the files in `out` import up front into `out`, as preload.json,
// and gives its path from `root`.
// The file holds the static imports of the files that the builds of
// `metafiles` wrote, and those of each of `vendors` that imports any, under
// the vendor file's name.
async function preloadList(
  root: string,
  out: string,
  metafiles: readonly esbuild.Metafile[],
  vendors: readonly Vendor[],
): Promise<string> {
  const lists: Record<string, readonly string[]> = {
    ...staticImports(metafiles),
    ...Object.fromEntries(
      vendors.filter((vendor) => vendor.imports.length > 0).map((vendor) => [vendor.file, vendor.imports] as const),
    ),
  };
  const file = join(out, "preload.json");
  const sorted = Object.entries(lists).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
  await writeFile(file, `${JSON.stringify(Object.fromEntries(sorted), null, 2)}\n`);
  return relative(root, file);
}

// Leaves each import of a vendored package in a module an import of the
// vendor file, `file`, by its name beside the module.
function vendorImports(packages: readonly string[], file: string): esbuild.Plugin {
  return {
    name: "blockr.dock-vendor-imports",
    setup(build) {
      build.onResolve({ filter: /^[^./]/ }, (args) =>
        packages.includes(args.path) ? { path: `./${file}`, external: true } : undefined,
      );
    },
  };
}

// Whether a path that a stylesheet imports names a file of one of the
// vendored packages, as dockview-react/dist/styles/dockview.css names one of
// dockview-react.
function vendoredFile(packages: readonly string[], path: string): boolean {
  return packages.some((name) => path.startsWith(`${name}/`));
}

// Leaves each `@import` of a vendored package's file in a stylesheet, as of
// dockview-react/dist/styles/dockview.css in the dock's, an `@import` of the
// path that `to` gives for it.
function vendorStylesheetImports(packages: readonly string[], to: (path: string) => string): esbuild.Plugin {
  return {
    name: "blockr.dock-vendor-stylesheets",
    setup(build) {
      build.onResolve({ filter: /^[^./]/ }, (args) =>
        args.kind === "import-rule" && vendoredFile(packages, args.path)
          ? { path: to(args.path), external: true }
          : undefined,
      );
    },
  };
}

// The files of the vendored packages that the stylesheets import, by the
// paths that they import them by, as dockview-react/dist/styles/dockview.css.
// It bundles the stylesheets without writing them, and reads the imports of
// such files that the bundles leave.
async function vendorStylesheets(
  root: string,
  out: string,
  stylesheets: readonly { readonly in: string; readonly out: string }[],
  packages: readonly string[],
): Promise<string[]> {
  const result = await esbuild.build({
    absWorkingDir: root,
    outdir: out,
    entryPoints: [...stylesheets],
    bundle: true,
    write: false,
    metafile: true,
    logLevel: "warning",
    plugins: [vendorStylesheetImports(packages, (path) => path)],
  });
  if (result.warnings.length > 0) {
    throw new Error("esbuild warned, as printed above");
  }

  const imports = Object.values(result.metafile.outputs).flatMap((output) =>
    output.imports.flatMap((entry) => (entry.external === true && vendoredFile(packages, entry.path) ? [entry.path] : [])),
  );
  return [...new Set(imports)];
}

// Writes the JSON Schema of the dock plugin's state, from the type DockState
// that srcts/state/types.ts exports, into `out` as dock.schema.json, beside
// the module of the dock plugin's class, and gives the file it wrote,
// relative to `root`.
// The schema allows no property that the types do not declare, and each type
// that the file exports is a definition of its own, as in core's schemas.
async function stateSchema(root: string, out: string): Promise<string[]> {
  const source = join(root, "srcts", "state", "types.ts");
  if (!existsSync(source)) return [];

  const schema = createGenerator({
    path: source,
    tsconfig: join(root, "tsconfig.client.json"),
    expose: "export",
    topRef: false,
    additionalProperties: false,
    skipTypeCheck: true,
  }).createSchema("DockState");

  const file = join(out, "dock.schema.json");
  await writeFile(file, `${JSON.stringify(schema, null, 2)}\n`);
  return [relative(root, file)];
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
