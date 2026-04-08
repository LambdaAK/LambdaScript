/**
 * Forge playground API: one long-lived `playground` process (see bin/playground.ml).
 * JSON responses on the child stderr; user [print]/[println] on stdout.
 *
 * Usage (from repo root): `node website/server/playground.mjs`
 * Or: `npm run server` from `website/`
 */

import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawn } from 'node:child_process';
import readline from 'node:readline';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(__dirname, '..', '..');
const PORT = Number(process.env.FORGE_PLAYGROUND_PORT ?? 8787);
const HOST = process.env.FORGE_PLAYGROUND_HOST ?? '127.0.0.1';
const intentionalShutdown = new WeakSet();

function playgroundBinaryPath() {
  const dir = path.join(repoRoot, '_build', 'default', 'bin');
  const withExe = path.join(dir, 'playground.exe');
  const noExe = path.join(dir, 'playground');
  if (fs.existsSync(withExe)) return withExe;
  if (fs.existsSync(noExe)) return noExe;
  throw new Error(
    'playground binary not found. From the repo root run: dune build bin/playground.exe'
  );
}

const binary = playgroundBinaryPath();

function spawnRunner() {
  const proc = spawn(binary, [], {
    cwd: repoRoot,
    stdio: ['pipe', 'pipe', 'pipe'],
    env: { ...process.env },
  });

  let stdoutAcc = '';
  proc.stdout.setEncoding('utf8');
  proc.stdout.on('data', (chunk) => {
    stdoutAcc += chunk;
  });

  const rl = readline.createInterface({ input: proc.stderr });

  proc.on('exit', (code, signal) => {
    if (intentionalShutdown.has(proc)) {
      intentionalShutdown.delete(proc);
      return;
    }
    console.error(`playground process exited code=${code} signal=${signal}`);
  });

  return { proc, rl, getStdout: () => stdoutAcc, clearStdout: () => { stdoutAcc = ''; } };
}

let runner = spawnRunner();

function shutdownRunner(currentRunner) {
  intentionalShutdown.add(currentRunner.proc);
  try {
    currentRunner.rl.close();
  } catch {}
  if (!currentRunner.proc.killed) currentRunner.proc.kill();
}

function restartRunner() {
  shutdownRunner(runner);
  runner = spawnRunner();
}

/** Serialize requests — one in-flight eval at a time (shared OCaml session). */
let queueTail = Promise.resolve();

function withQueue(fn) {
  const run = queueTail.then(fn, fn);
  queueTail = run.then(
    () => {},
    () => {}
  );
  return run;
}

function runSnippet(code) {
  return new Promise((resolve, reject) => {
    const currentRunner = runner;
    currentRunner.clearStdout();
    const buf = Buffer.from(code, 'utf8');
    if (buf.length > 4 * 1024 * 1024) {
      reject(new Error('Code exceeds 4 MiB'));
      return;
    }
    const header = Buffer.alloc(4);
    header.writeUInt32BE(buf.length, 0);
    const payload = Buffer.concat([header, buf]);

    const onLine = (line) => {
      try {
        const json = JSON.parse(line);
        json.printed = currentRunner.getStdout();
        resolve(json);
      } catch (e) {
        reject(e);
      }
    };
    currentRunner.rl.once('line', onLine);

    currentRunner.proc.stdin.write(payload, (err) => {
      if (err) {
        currentRunner.rl.off('line', onLine);
        reject(err);
      }
    });
  });
}

function json(res, status, body) {
  const data = JSON.stringify(body);
  res.writeHead(status, {
    'Content-Type': 'application/json; charset=utf-8',
    'Content-Length': Buffer.byteLength(data),
  });
  res.end(data);
}

const server = http.createServer(async (req, res) => {
  if (req.method === 'OPTIONS') {
    res.writeHead(204, {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'POST, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type',
    });
    res.end();
    return;
  }

  if (req.url === '/health' && req.method === 'GET') {
    json(res, 200, { ok: true, pid: runner.proc.pid });
    return;
  }

  if (req.url === '/reset' && req.method === 'POST') {
    try {
      await withQueue(async () => {
        restartRunner();
      });
      json(res, 200, { ok: true });
    } catch (e) {
      json(res, 500, { ok: false, error: e instanceof Error ? e.message : String(e) });
    }
    return;
  }

  if (req.url !== '/run' || req.method !== 'POST') {
    json(res, 404, { ok: false, error: 'Not found' });
    return;
  }

  let body = '';
  for await (const chunk of req) {
    body += chunk;
    if (body.length > 4 * 1024 * 1024) {
      json(res, 413, { ok: false, error: 'Body too large' });
      return;
    }
  }

  let parsed;
  try {
    parsed = JSON.parse(body);
  } catch {
    json(res, 400, { ok: false, error: 'Invalid JSON' });
    return;
  }

  const code = typeof parsed.code === 'string' ? parsed.code : '';
  try {
    const out = await withQueue(() => runSnippet(code));
    json(res, 200, out);
  } catch (e) {
    json(res, 500, { ok: false, error: e instanceof Error ? e.message : String(e) });
  }
});

server.listen(PORT, HOST, () => {
  console.error(`Forge playground server http://${HOST}:${PORT}`);
});
