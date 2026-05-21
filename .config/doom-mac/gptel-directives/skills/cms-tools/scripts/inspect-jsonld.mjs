#!/usr/bin/env node
// inspect-jsonld.mjs <url> [--variation A|B|C|...]
//
// Read-only. Fetches a URL (raw HTML, no JS execution, no cookies), extracts
// every <script type="application/ld+json"> block, and reports the parsed
// schemas + any validity issues. If --variation is provided, the URL is
// re-fetched with `?v=<key>&xid=<experimentId>` overrides so you can probe
// each A/B/C variation independently.
//
// This is the "Verification = raw HTML only" path from the JSON-LD session —
// what an SEO crawler actually sees, not what Mongo says it should see.
//
// Examples:
//   node inspect-jsonld.mjs http://localhost:3000/colorbar/locations
//   node inspect-jsonld.mjs https://www.madison-reed.com/shop/brown
//   node inspect-jsonld.mjs http://localhost:3000/colorbar/location-specific --variation B
//
// See: rules/json-ld-management.md (R1/R2/R3 paths and per-variation probing).

import { parseArgs, printJson, die, requirePositional } from './lib/mongo.mjs';

const { positional, options } = parseArgs(process.argv);
requirePositional(positional, 1, 'inspect-jsonld.mjs <url> [--variation X]');

const inputUrl = positional[0];
const variation = options['--variation'] || null;

async function fetchHtml(url) {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), 30000);
  try {
    const r = await fetch(url, {
      headers: { 'User-Agent': 'Mozilla/5.0 (tophat-tools/inspect-jsonld)' },
      signal: controller.signal,
      redirect: 'follow',
    });
    return { status: r.status, html: await r.text() };
  } finally {
    clearTimeout(timer);
  }
}

const SCRIPT_RE = /<script\b[^>]*\btype\s*=\s*["']application\/ld\+json["'][^>]*>([\s\S]*?)<\/script>/gi;
const EXP_ID_RE = /"experimentId"\s*:\s*"?(\d+)"?/;
const EXP_NAME_RE = /"experimentName"\s*:\s*"([^"]+)"/;
const VAR_KEY_RE = /"variationKey"\s*:\s*"([^"]+)"/;

function extractJsonLd(html) {
  const blocks = [];
  let m;
  let i = 0;
  while ((m = SCRIPT_RE.exec(html)) !== null) {
    const text = m[1];
    let parsed = null;
    let parseError = null;
    try {
      parsed = JSON.parse(text);
    } catch (e) {
      parseError = e.message;
    }
    blocks.push({
      index: i++,
      parseError,
      parsed,
      types: collectTypes(parsed),
      raw: text.length > 200 ? `${text.slice(0, 200)}…` : text,
    });
  }
  return blocks;
}

function collectTypes(node, acc = new Set()) {
  if (!node || typeof node !== 'object') {
    return Array.from(acc);
  }
  if (Array.isArray(node)) {
    node.forEach((c) => collectTypes(c, acc));
    return Array.from(acc);
  }
  if (typeof node['@type'] === 'string') {
    acc.add(node['@type']);
  } else if (Array.isArray(node['@type'])) {
    node['@type'].forEach((t) => acc.add(t));
  }
  Object.keys(node).forEach((k) => {
    if (k !== '@type' && k !== '@context') {
      collectTypes(node[k], acc);
    }
  });
  return Array.from(acc);
}

function detectExperiment(html) {
  const id = html.match(EXP_ID_RE);
  const name = html.match(EXP_NAME_RE);
  const key = html.match(VAR_KEY_RE);
  if (!id || !key) {
    return null;
  }
  return {
    experimentId: id[1],
    experimentName: name ? name[1] : null,
    currentVariationKey: key[1],
  };
}

function urlWithVariation(url, key, experimentId) {
  const u = new URL(url);
  u.searchParams.set('v', key);
  u.searchParams.set('xid', String(experimentId));
  return u.toString();
}

(async () => {
  let url = inputUrl;
  let probedVariation = null;

  if (variation) {
    const base = await fetchHtml(url);
    const exp = detectExperiment(base.html);
    if (!exp) {
      die(`URL "${url}" has no detectable experiment markers — cannot force variation "${variation}"`);
    }
    url = urlWithVariation(url, variation, exp.experimentId);
    probedVariation = {
      forced: variation,
      experimentId: exp.experimentId,
      experimentName: exp.experimentName,
    };
  }

  const { status, html } = await fetchHtml(url);
  if (status !== 200) {
    die(`HTTP ${status} for ${url}`);
  }

  const exp = detectExperiment(html);
  const blocks = extractJsonLd(html);

  printJson({
    url,
    httpStatus: status,
    probedVariation,
    activeExperiment: exp,
    jsonLdCount: blocks.length,
    schemaTypes: Array.from(new Set(blocks.flatMap((b) => b.types))),
    blocks,
  });
})().catch((e) => die(e.message));
