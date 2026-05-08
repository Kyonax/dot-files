// Shared mongosh wrapper for the tophat-tools skill.
//
// All scripts in this skill talk to MongoDB through `docker exec mr-mongo mongosh cms`.
// CMS database name is `cms` (NOT `madisonreed`).
//
// Two ways to query:
//
//   import { mongoEval, mongoJson } from './lib/mongo.mjs';
//
//   // Run an arbitrary mongosh expression and capture stdout (string).
//   mongoEval('db.content.findOne({_id: 3117}).uri');
//
//   // Wrap a function body so the result is JSON-serialised via EJSON and parsed
//   // back as a plain JS object. Use this for objects, arrays, and complex shapes
//   // because the default `print(...)` of a BSON object is not JSON-parseable.
//   const doc = mongoJson('return db.content.findOne({_id: 3117});');
//
// All scripts honour the `--container <name>` and `--db <name>` flags, defaulting
// to `mr-mongo` and `cms`. Override via the args struct returned by parseArgs.

import { spawnSync } from 'node:child_process';

const DEFAULT_CONTAINER = 'mr-mongo';
const DEFAULT_DB = 'cms';
const MAX_BUFFER = 50 * 1024 * 1024;

let _container = DEFAULT_CONTAINER;
let _db = DEFAULT_DB;

export function setMongoTarget({ container, db } = {}) {
  if (container) {
    _container = container;
  }
  if (db) {
    _db = db;
  }
}

export function mongoEval(js, { timeoutMs = 30000 } = {}) {
  const r = spawnSync(
    'docker',
    ['exec', _container, 'mongosh', _db, '--quiet', '--eval', js],
    { encoding: 'utf8', maxBuffer: MAX_BUFFER, timeout: timeoutMs }
  );
  if (r.error) {
    throw new Error(`docker exec failed: ${r.error.message}`);
  }
  if (r.status !== 0) {
    throw new Error(
      `mongosh exited ${r.status}\nstderr: ${r.stderr || '(empty)'}\nstdout: ${r.stdout || '(empty)'}`
    );
  }
  return r.stdout;
}

export function mongoJson(jsBody, opts) {
  const wrapped = `print(EJSON.stringify((function(){ ${jsBody} })(), null, 0))`;
  const stdout = mongoEval(wrapped, opts);
  // mongosh prints a trailing newline; trim whitespace only.
  const trimmed = stdout.trim();
  if (!trimmed) {
    return null;
  }
  try {
    return JSON.parse(trimmed);
  } catch (e) {
    throw new Error(
      `mongoJson: failed to parse mongosh stdout as JSON.\nFirst 200 chars: ${trimmed.slice(0, 200)}\nParse error: ${e.message}`
    );
  }
}

// Convenience helpers used across multiple scripts.

export function findContentById(contentId, projection = null) {
  const proj = projection ? `, ${JSON.stringify(projection)}` : '';
  return mongoJson(`return db.content.findOne({_id: ${Number(contentId)}}${proj});`);
}

export function findContentByUri(uri) {
  // Walk parent paths so /shop/brown can resolve to /shop/* via takesUrlParameters.
  const candidates = uriCandidates(uri);
  const docs = mongoJson(
    `return db.content.find({uri: {$in: ${JSON.stringify(candidates)}}}, {_id:1, uri:1, takesUrlParameters:1, name:1}).toArray();`
  );
  if (!docs || docs.length === 0) {
    return null;
  }
  // Prefer exact match, fallback to a parent with takesUrlParameters=true.
  const exact = docs.find((d) => d.uri === uri || d.uri === uri + '/' || d.uri === uri.replace(/\/$/, ''));
  if (exact) {
    return { ...exact, resolvedVia: 'exact' };
  }
  const parametric = docs.find((d) => d.takesUrlParameters);
  if (parametric) {
    return { ...parametric, resolvedVia: 'takesUrlParameters' };
  }
  return null;
}

export function findContentVersions(contentId, version = null, projection = null) {
  const verFilter = version != null ? `, version: ${Number(version)}` : '';
  const proj = projection ? `, ${JSON.stringify(projection)}` : '';
  return mongoJson(
    `return db.contentVersion.find({content_id: ${Number(contentId)}${verFilter}}${proj}).toArray();`
  );
}

export function findTemplateByMixinKey(mixinKey, projection = null) {
  const proj = projection ? `, ${JSON.stringify(projection)}` : '';
  return mongoJson(
    `return db.template.findOne({mixin_key: ${JSON.stringify(mixinKey)}}${proj});`
  );
}

export function findTemplateById(templateId, projection = null) {
  const proj = projection ? `, ${JSON.stringify(projection)}` : '';
  return mongoJson(`return db.template.findOne({_id: ${Number(templateId)}}${proj});`);
}

export function findTemplateVersion(templateId, version) {
  return mongoJson(
    `return db.templateVersion.findOne({template_id: ${Number(templateId)}, version: ${Number(version)}});`
  );
}

export function findExperiment(experimentDocId) {
  return mongoJson(`return db.experiment.findOne({_id: ${Number(experimentDocId)}});`);
}

export function findExperimentByName(name) {
  return mongoJson(`return db.experiment.findOne({name: ${JSON.stringify(name)}});`);
}

// Standard CLI arg parser. Returns:
//   { positional: [...], flags: Set<string>, options: { '--key': 'value', ... } }
export function parseArgs(argv) {
  const args = argv.slice(2);
  const positional = [];
  const flags = new Set();
  const options = {};
  for (let i = 0; i < args.length; i += 1) {
    const a = args[i];
    if (a.startsWith('--')) {
      const next = args[i + 1];
      if (next === undefined || next.startsWith('--')) {
        flags.add(a);
      } else {
        options[a] = next;
        i += 1;
      }
    } else {
      positional.push(a);
    }
  }
  return { positional, flags, options };
}

export function applyMongoFlags(options) {
  setMongoTarget({
    container: options['--container'],
    db: options['--db'],
  });
}

export function uriCandidates(uri) {
  const out = new Set([uri]);
  if (!uri.endsWith('/')) {
    out.add(uri + '/');
  } else {
    out.add(uri.replace(/\/$/, ''));
  }
  let p = uri.replace(/\/$/, '');
  while (true) {
    const i = p.lastIndexOf('/');
    if (i <= 0) {
      break;
    }
    p = p.substring(0, i);
    if (!p) {
      break;
    }
    out.add(p);
    out.add(p + '/');
  }
  return Array.from(out);
}

export function printJson(value) {
  console.log(JSON.stringify(value, null, 2));
}

export function die(message, code = 1) {
  console.error(`ERROR: ${message}`);
  process.exit(code);
}

export function requirePositional(positional, n, usage) {
  if (positional.length < n) {
    console.error(`USAGE: ${usage}`);
    process.exit(2);
  }
}
