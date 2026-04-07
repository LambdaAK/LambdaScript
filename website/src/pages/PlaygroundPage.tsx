import { useCallback, useState } from 'react'
import { Link } from 'react-router-dom'

const defaultCode = 'let x = 40 in x + 2'

type RunResponse =
  | { ok: true; kind: 'expr'; value: string; type: string; printed?: string }
  | {
      ok: true
      kind: 'defs'
      bindings: { name: string; value: string; type: string }[]
      printed?: string
    }
  | { ok: false; error: string; printed?: string }

function formatResult(data: RunResponse): string {
  if (!data.ok) {
    return data.error
  }
  const printed = data.printed?.trim() ? `[stdout]\n${data.printed}\n\n` : ''
  if (data.kind === 'expr') {
    return `${printed}${data.value} : ${data.type}`
  }
  const lines = data.bindings.map((b) => `${b.name} = ${b.value} : ${b.type}`)
  return `${printed}${lines.join('\n')}`
}

function browserEngineAvailable(): boolean {
  return typeof window.ForgePlayground?.eval === 'function'
}

function parseRunJson(text: string, label: string): RunResponse | null {
  const t = text.trim()
  if (!t) return null
  try {
    return JSON.parse(t) as RunResponse
  } catch {
    throw new Error(`${label}: invalid JSON:\n${t.slice(0, 400)}${t.length > 400 ? '…' : ''}`)
  }
}

export default function PlaygroundPage() {
  const [code, setCode] = useState(defaultCode)
  const [out, setOut] = useState('')
  const [err, setErr] = useState(false)
  const [busy, setBusy] = useState(false)
  const [engine] = useState<'browser' | 'server'>(() =>
    typeof window !== 'undefined' && browserEngineAvailable() ? 'browser' : 'server'
  )

  const resetSession = useCallback(() => {
    window.ForgePlayground?.reset?.()
    setOut('')
    setErr(false)
  }, [])

  const run = useCallback(async () => {
    setBusy(true)
    setErr(false)
    setOut('Running…')
    try {
      if (browserEngineAvailable()) {
        const raw = window.ForgePlayground!.eval(code)
        const data = parseRunJson(raw, 'ForgePlayground.eval')
        if (!data) {
          setErr(true)
          setOut('Empty result from in-browser evaluator.')
          return
        }
        if (data.ok === false) {
          setErr(true)
          setOut(data.error)
          return
        }
        setOut(formatResult(data))
        return
      }

      const res = await fetch('/api/run', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ code }),
      })
      const text = await res.text()
      const data = parseRunJson(text, '/api/run')
      if (!data) {
        setErr(true)
        setOut(
          'Empty response from /api/run. For local dev, run:\n' +
            '  npm run server --prefix website\n' +
            'Or build the in-browser bundle:\n' +
            '  dune build browser/forge_browser.bc.js && npm run sync:forge-js --prefix website'
        )
        return
      }
      if (!res.ok) {
        setErr(true)
        const er = data as { error?: string }
        setOut(typeof er.error === 'string' ? er.error : `HTTP ${res.status}`)
        return
      }
      if (data.ok === false) {
        setErr(true)
        setOut(typeof (data as { error?: string }).error === 'string' ? (data as { error: string }).error : 'Error')
        return
      }
      setOut(formatResult(data))
    } catch (e) {
      setErr(true)
      setOut(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }, [code])

  return (
    <div>
      <h1>Playground</h1>
      <p style={{ color: '#b8c0cc', maxWidth: '42rem' }}>
        {engine === 'browser' ? (
          <>
            Running Forge in your browser via <strong>js_of_ocaml</strong> (same prelude and session
            semantics as the REPL). No server required.
          </>
        ) : (
          <>
            In-browser evaluator not loaded. Use the <strong>local API</strong> (see{' '}
            <Link to="/docs/install">Install</Link>) or build the bundle:{' '}
            <code>dune build browser/forge_browser.bc.js</code> then{' '}
            <code>npm run sync:forge-js --prefix website</code>.
          </>
        )}
      </p>
      <div className="playground-grid" style={{ marginTop: '1rem' }}>
        <div>
          <textarea
            className="playground-editor"
            value={code}
            onChange={(e) => setCode(e.target.value)}
            spellCheck={false}
            aria-label="Forge source"
          />
          <div style={{ marginTop: '0.5rem', display: 'flex', gap: '0.5rem', flexWrap: 'wrap' }}>
            <button type="button" className="playground-run" disabled={busy} onClick={() => void run()}>
              Run
            </button>
            {engine === 'browser' ? (
              <button
                type="button"
                className="btn"
                disabled={busy}
                onClick={() => resetSession()}
                title="Clear prelude session (re-run prelude on next eval)"
              >
                Reset session
              </button>
            ) : null}
          </div>
        </div>
        <div>
          <div className={`playground-out ${err ? 'err' : ''}`} aria-live="polite">
            {err ? <span className="err">{out}</span> : <span className="ok-type">{out}</span>}
          </div>
        </div>
      </div>
    </div>
  )
}
