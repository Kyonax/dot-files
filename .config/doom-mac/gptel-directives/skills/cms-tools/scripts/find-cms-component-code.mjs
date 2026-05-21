#!/usr/bin/env node
// find-cms-component-code.mjs <mixin_key|tag-name> [--repo <path>]
//
// Read-only (filesystem only — no Mongo). Locates the Vue component code
// behind a CMS mixin_key. The convention in this codebase:
//
//   1. CMS mixin_key (kebab-case, e.g. `location-specific-colorbar-v2`)
//      maps to a globally registered Vue tag with the same kebab-case name.
//   2. Global registration happens in:
//      - website/src/vuescripts/mrVueApp.js          (client)
//      - website/src/vuescripts/ssr/registerGlobalsSsr.js  (SSR)
//   3. The PascalCase component name registered there points to a path
//      under @components/, which is `website/src/vuescripts/components/`.
//
// Output: { mixin_key, pascalName, registrations: [...], files: [...] }
//
// Examples:
//   node find-cms-component-code.mjs location-specific-colorbar-v2
//   node find-cms-component-code.mjs hcb-landing-sticky
//
// See: rules/code-locator-scripts.md.

import { spawnSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import { join } from 'node:path';

import { parseArgs, printJson, die, requirePositional } from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
requirePositional(positional, 1, 'find-cms-component-code.mjs <mixin_key|tag-name> [--repo <path>]');

const mixinKey = positional[0].toLowerCase();
const repoRoot = options['--repo'] || guessRepoRoot();

if (!repoRoot) {
  die(
    'Could not auto-detect MR repo root. Pass --repo /path/to/the-code or run from inside the repo.'
  );
}

function guessRepoRoot() {
  const candidates = [
    process.cwd(),
    '/Volumes/dev-partition/github-madison-reed/the-code',
  ];
  for (const c of candidates) {
    if (existsSync(join(c, 'website/src/vuescripts/mrVueApp.js'))) {
      return c;
    }
  }
  // Walk up from cwd.
  let p = process.cwd();
  for (let i = 0; i < 10; i += 1) {
    if (existsSync(join(p, 'website/src/vuescripts/mrVueApp.js'))) {
      return p;
    }
    const parent = join(p, '..');
    if (parent === p) {
      break;
    }
    p = parent;
  }
  return null;
}

function pascalCase(kebab) {
  return kebab
    .split('-')
    .filter(Boolean)
    .map((p) => p.charAt(0).toUpperCase() + p.slice(1))
    .join('');
}

function grep(pattern, file) {
  const r = spawnSync('grep', ['-n', '-E', pattern, file], { encoding: 'utf8' });
  if (r.status === 1 || !r.stdout) {
    return [];
  }
  if (r.status !== 0) {
    return [];
  }
  return r.stdout.trim().split('\n');
}

const pascalName = pascalCase(mixinKey);

const clientReg = join(repoRoot, 'website/src/vuescripts/mrVueApp.js');
const ssrReg = join(repoRoot, 'website/src/vuescripts/ssr/registerGlobalsSsr.js');

// Look for the kebab name AND the PascalCase name in both registration files.
const registrations = [];
[clientReg, ssrReg].forEach((file) => {
  if (!existsSync(file)) {
    return;
  }
  const matches = [
    ...grep(`['"\`]${pascalName}['"\`]`, file),
    ...grep(mixinKey, file),
  ];
  matches.forEach((m) => registrations.push({ file: file.replace(repoRoot + '/', ''), line: m }));
});

// Find the actual file(s) on disk.
const componentSearch = spawnSync(
  'find',
  [
    join(repoRoot, 'website/src/vuescripts/components'),
    '-type',
    'f',
    '-iname',
    `${pascalName}.vue`,
  ],
  { encoding: 'utf8' }
);

const files = (componentSearch.stdout || '')
  .trim()
  .split('\n')
  .filter(Boolean)
  .map((p) => p.replace(repoRoot + '/', ''));

// Also check for the kebab folder convention.
const kebabFind = spawnSync(
  'find',
  [
    join(repoRoot, 'website/src/vuescripts/components'),
    '-type',
    'd',
    '-iname',
    pascalName,
  ],
  { encoding: 'utf8' }
);
const folders = (kebabFind.stdout || '')
  .trim()
  .split('\n')
  .filter(Boolean)
  .map((p) => p.replace(repoRoot + '/', ''));

printJson({
  mixin_key: mixinKey,
  pascalName,
  registrations,
  componentFiles: files,
  componentFolders: folders,
  registrationHint:
    registrations.length === 0
      ? `No global registration found. The mixin_key "${mixinKey}" may render a CMS partial via <cms-partial mixin-key="${mixinKey}">, in which case the body is HTML configured in Tophat (template.jade) — check inspect-template.mjs ${mixinKey}.`
      : null,
});
