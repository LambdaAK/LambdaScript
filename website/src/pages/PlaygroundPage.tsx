import CodeMirror, { minimalSetup } from '@uiw/react-codemirror'
import { EditorView } from '@codemirror/view'
import { useCallback, useState } from 'react'
import { Link } from 'react-router-dom'
import { forgeLanguage } from '../forge/forgeLanguage'
import { forgeEditorChrome, forgeSyntaxHighlighting } from '../forge/forgeTheme'
import {
  DEFAULT_PLAYGROUND_EXAMPLE_ID,
  findPlaygroundExample,
  PLAYGROUND_EXAMPLES,
} from '../playgroundExamples'

const CUSTOM_EXAMPLE_ID = '__custom__'

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

const forgeExtensions = [
  minimalSetup({ syntaxHighlighting: false }),
  forgeLanguage,
  forgeSyntaxHighlighting,
  forgeEditorChrome,
  EditorView.lineWrapping,
]

export default function PlaygroundPage() {
  const initial = findPlaygroundExample(DEFAULT_PLAYGROUND_EXAMPLE_ID) ?? PLAYGROUND_EXAMPLES[0]
  const [exampleId, setExampleId] = useState<string>(initial.id)
  const [code, setCode] = useState(initial.code)
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

  const loadExample = useCallback(
    (id: string) => {
      if (id === CUSTOM_EXAMPLE_ID) return
      const ex = findPlaygroundExample(id)
      if (!ex) return
      setExampleId(id)
      setCode(ex.code)
      setOut('')
      setErr(false)
      window.ForgePlayground?.reset?.()
    },
    []
  )

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
          <label className="playground-examples-label">
            <span>Examples</span>
            <select
              className="playground-examples-select"
              aria-label="Load example program"
              value={exampleId}
              onChange={(e) => loadExample(e.target.value)}
            >
              {PLAYGROUND_EXAMPLES.map((ex) => (
                <option key={ex.id} value={ex.id} title={ex.description}>
                  {ex.label}
                </option>
              ))}
              {exampleId === CUSTOM_EXAMPLE_ID ? (
                <option value={CUSTOM_EXAMPLE_ID}>Custom (your edit)</option>
              ) : null}
            </select>
          </label>
          <div className="playground-editor-host">
            <CodeMirror
              theme="none"
              basicSetup={false}
              value={code}
              minHeight="220px"
              className="playground-editor"
              extensions={forgeExtensions}
              onChange={(v) => {
                setCode(v)
                setExampleId(CUSTOM_EXAMPLE_ID)
              }}
              editable
              indentWithTab
              spellCheck={false}
              aria-label="Forge source"
            />
          </div>
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
