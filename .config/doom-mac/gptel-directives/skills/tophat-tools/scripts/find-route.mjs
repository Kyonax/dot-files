#!/usr/bin/env node
// find-route.mjs <uri> [--repo <path>]
//
// Read-only (filesystem only — no Mongo). Greps the Express routing layer for
// handlers that match the given URI. The MR routing pattern:
//
//   - website/src/routing/routing.js — top-level mounts
//   - website/src/routing/views.js   — page handlers (CMS-driven and Pug)
//   - website/src/routing/endpoints.js — API endpoints (/api/...)
//   - website/src/routing/before.js + contexts.js — request-context middleware
//
// Most CMS pages don't have a dedicated route — they go through the catch-all
// in `views.js` that calls `mr_modules/cms/lib/loaders.js` to resolve content
// by URI. find-route.mjs reports any direct route handler matches AND warns
// when the URI is likely served via the CMS catch-all.
//
// Examples:
//   node find-route.mjs /colorbar/locations
//   node find-route.mjs /api/cmsSvc/getPartial
//
// See: rules/code-locator-scripts.md.

import { spawnSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import { join } from 'node:path';

import { parseArgs, printJson, die, requirePositional } from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
requirePositional(positional, 1, 'find-route.mjs <uri> [--repo <path>]');

const uri = positional[0];
const repoRoot = options['--repo'] || guessRepoRoot();
if (!repoRoot) {
  die('Could not auto-detect MR repo root. Pass --repo /path/to/the-code.');
}

function guessRepoRoot() {
  if (existsSync(join(process.cwd(), 'website/src/routing/routing.js'))) {
    return process.cwd();
  }
  const fallback = '/Volumes/dev-partition/github-madison-reed/the-code';
  if (existsSync(join(fallback, 'website/src/routing/routing.js'))) {
    return fallback;
  }
  return null;
}

const routingFiles = [
  'website/src/routing/routing.js',
  'website/src/routing/before.js',
  'website/src/routing/contexts.js',
  'website/src/routing/views.js',
  'website/src/routing/endpoints.js',
];

function grepFile(file, term) {
  const r = spawnSync('grep', ['-n', '-F', term, file], { encoding: 'utf8' });
  if (r.status !== 0 || !r.stdout) {
    return [];
  }
  return r.stdout.trim().split('\n');
}

const matches = [];
const escaped = uri.replace(/^\//, '').replace(/\/$/, '');
for (const rel of routingFiles) {
  const full = join(repoRoot, rel);
  if (!existsSync(full)) {
    continue;
  }
  const hits = [...grepFile(full, uri), ...grepFile(full, escaped)];
  hits.forEach((h) => matches.push({ file: rel, line: h }));
}

// Also look in the endpoints subdirectory.
const endpointsDir = join(repoRoot, 'website/src/routing/endpoints');
if (existsSync(endpointsDir)) {
  const r = spawnSync(
    'grep',
    ['-rn', '-F', uri, endpointsDir],
    { encoding: 'utf8' }
  );
  if (r.status === 0 && r.stdout) {
    r.stdout
      .trim()
      .split('\n')
      .forEach((l) => {
        const [path, ...rest] = l.split(':');
        matches.push({ file: path.replace(repoRoot + '/', ''), line: rest.join(':') });
      });
  }
}

const isLikelyCms =
  matches.length === 0 ||
  matches.every((m) => m.file === 'website/src/routing/views.js');

printJson({
  uri,
  totalMatches: matches.length,
  matches,
  cmsCatchAllNote: isLikelyCms
    ? `${uri} is likely served via the CMS catch-all in views.js (resolves content by URI through mr_modules/cms/lib/loaders.js#getContent). Use inspect-content-by-uri.mjs ${uri} to see the matching content document.`
    : null,
});
