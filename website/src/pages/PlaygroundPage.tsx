import CodeMirror, { minimalSetup } from '@uiw/react-codemirror'
import { EditorView } from '@codemirror/view'
import { useCallback, useEffect, useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import { forgeLanguage } from '../forge/forgeLanguage'
import { forgeEditorChrome, forgeSyntaxHighlighting } from '../forge/forgeTheme'
import {
  DEFAULT_PLAYGROUND_PROJECT_ID,
  findPlaygroundProject,
  PLAYGROUND_PROJECTS,
  type PlaygroundProject,
} from '../playgroundExamples'

const CUSTOM_PROJECT_ID = '__custom__'
const NEW_FILE_TEMPLATE = 'let _ = println "New Forge file"'
const LOCAL_API_HELP =
  'Local playground API is unavailable.\n' +
  'Start it with:\n' +
  '  dune build bin/playground.exe\n' +
  '  npm run server --prefix website\n' +
  'Or build the in-browser bundle:\n' +
  '  dune build browser/forge_browser.bc.js && npm run sync:forge-js --prefix website'

type RunResponse =
  | { ok: true; kind: 'expr'; value: string; type: string; printed?: string }
  | {
      ok: true
      kind: 'defs'
      bindings: { name: string; value: string; type: string }[]
      printed?: string
    }
  | { ok: false; error: string; printed?: string }

type SessionMode = 'isolated' | 'sticky'
type OutputTab = 'result' | 'stdout'
type RunTarget = 'active' | 'entry' | 'workspace' | `file:${string}`

type IdeFile = {
  id: string
  name: string
  code: string
}

type WorkspaceState = {
  projectId: string
  files: IdeFile[]
  activeFileId: string
  entryFileId: string
}

let fileIdSeed = 0

function nextFileId(): string {
  fileIdSeed += 1
  return `forge-file-${fileIdSeed.toString(36)}`
}

function browserEngineAvailable(): boolean {
  return typeof window.ForgePlayground?.eval === 'function'
}

function parseJson<T>(text: string, label: string): T | null {
  const trimmed = text.trim()
  if (!trimmed) return null
  try {
    return JSON.parse(trimmed) as T
  } catch {
    throw new Error(`${label}: invalid JSON:\n${trimmed.slice(0, 400)}${trimmed.length > 400 ? '…' : ''}`)
  }
}

function parseRunJson(text: string, label: string): RunResponse | null {
  return parseJson<RunResponse>(text, label)
}

function ensureForgeFileName(raw: string): string {
  const cleaned = raw.trim().replace(/\s+/g, '-').replace(/[^a-zA-Z0-9._-]/g, '')
  const base = cleaned.length > 0 ? cleaned : 'module'
  return base.endsWith('.forge') ? base : `${base}.forge`
}

function dedupeFileName(proposed: string, existing: string[]): string {
  if (!existing.includes(proposed)) return proposed
  const dot = proposed.lastIndexOf('.')
  const stem = dot > 0 ? proposed.slice(0, dot) : proposed
  const ext = dot > 0 ? proposed.slice(dot) : ''
  let index = 2
  let candidate = `${stem}-${index}${ext}`
  while (existing.includes(candidate)) {
    index += 1
    candidate = `${stem}-${index}${ext}`
  }
  return candidate
}

function copiedFileName(name: string): string {
  const stem = name.endsWith('.forge') ? name.slice(0, -'.forge'.length) : name
  return `${stem}-copy.forge`
}

function formatResult(data: RunResponse): string {
  if (!data.ok) return data.error
  if (data.kind === 'expr') return `${data.value} : ${data.type}`
  return data.bindings.map((binding) => `${binding.name} = ${binding.value} : ${binding.type}`).join('\n')
}

function defaultWorkspace(): WorkspaceState {
  const project = findPlaygroundProject(DEFAULT_PLAYGROUND_PROJECT_ID) ?? PLAYGROUND_PROJECTS[0]
  const files = project.files.map((file) => ({ id: nextFileId(), name: file.name, code: file.code }))
  const firstFileId = files[0]?.id ?? nextFileId()
  const entryFile = files.find((file) => file.name === project.entryFile) ?? files[0]

  return {
    projectId: project.id,
    files: files.length > 0 ? files : [{ id: firstFileId, name: 'main.forge', code: NEW_FILE_TEMPLATE }],
    activeFileId: firstFileId,
    entryFileId: entryFile?.id ?? firstFileId,
  }
}

function instantiateProject(project: PlaygroundProject): WorkspaceState {
  const files = project.files.map((file) => ({ id: nextFileId(), name: file.name, code: file.code }))
  const firstFileId = files[0]?.id ?? nextFileId()
  const entryFile = files.find((file) => file.name === project.entryFile) ?? files[0]
  return {
    projectId: project.id,
    files: files.length > 0 ? files : [{ id: firstFileId, name: 'main.forge', code: NEW_FILE_TEMPLATE }],
    activeFileId: firstFileId,
    entryFileId: entryFile?.id ?? firstFileId,
  }
}

function normalizeWorkspace(state: WorkspaceState): WorkspaceState {
  const files = state.files.length > 0 ? state.files : [{ id: nextFileId(), name: 'main.forge', code: NEW_FILE_TEMPLATE }]
  const activeFileId = files.some((file) => file.id === state.activeFileId) ? state.activeFileId : files[0].id
  const entryFileId = files.some((file) => file.id === state.entryFileId) ? state.entryFileId : files[0].id
  return {
    ...state,
    files,
    activeFileId,
    entryFileId,
  }
}

function assembleWorkspace(files: IdeFile[]): string {
  return files
    .map((file) => {
      const body = file.code.trim().length > 0 ? file.code : '// empty file'
      return `// ===== ${file.name} =====\n${body}`
    })
    .join('\n\n')
}

function resolveRunSnippet(workspace: WorkspaceState, target: RunTarget): { label: string; code: string } | null {
  const findFile = (fileId: string) => workspace.files.find((file) => file.id === fileId)

  if (target.startsWith('file:')) {
    const file = findFile(target.slice('file:'.length))
    if (!file) return null
    return { label: `File ${file.name}`, code: file.code }
  }

  if (target === 'workspace') {
    return {
      label: `Workspace (${workspace.files.length} files)`,
      code: assembleWorkspace(workspace.files),
    }
  }

  if (target === 'entry') {
    const entry = findFile(workspace.entryFileId)
    if (!entry) return null
    const withEntryLast = [...workspace.files.filter((file) => file.id !== entry.id), entry]
    return {
      label: `Entry ${entry.name} (+ workspace context)`,
      code: assembleWorkspace(withEntryLast),
    }
  }

  const active = findFile(workspace.activeFileId)
  if (!active) return null
  return { label: `Active ${active.name}`, code: active.code }
}

async function runOnServer(code: string): Promise<RunResponse> {
  try {
    const response = await fetch('/api/run', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ code }),
    })

    const text = await response.text()
    const parsed = parseRunJson(text, '/api/run')

    if (!parsed) {
      if (!response.ok) throw new Error(LOCAL_API_HELP)
      throw new Error(`Empty response from /api/run.\n\n${LOCAL_API_HELP}`)
    }

    if (!response.ok) {
      const message =
        typeof (parsed as { error?: unknown }).error === 'string'
          ? (parsed as { error: string }).error
          : `HTTP ${response.status}`
      if (response.status >= 500 && message.startsWith('HTTP')) throw new Error(LOCAL_API_HELP)
      throw new Error(message)
    }

    return parsed
  } catch (error) {
    if (error instanceof Error && (error.message.includes('Failed to fetch') || error.message.includes('NetworkError'))) {
      throw new Error(LOCAL_API_HELP)
    }
    throw error
  }
}

async function resetServerSession(): Promise<void> {
  try {
    const response = await fetch('/api/reset', {
      method: 'POST',
    })

    if (response.ok) return

    const text = await response.text()
    const parsed =
      text.trim().length === 0
        ? null
        : (() => {
            try {
              return parseJson<{ error?: string }>(text, '/api/reset')
            } catch {
              return null
            }
          })()

    const message = typeof parsed?.error === 'string' ? parsed.error : `Reset failed: HTTP ${response.status}`
    if (response.status >= 500 && message.startsWith('Reset failed: HTTP')) throw new Error(LOCAL_API_HELP)
    throw new Error(message)
  } catch (error) {
    if (error instanceof Error && (error.message.includes('Failed to fetch') || error.message.includes('NetworkError'))) {
      throw new Error(LOCAL_API_HELP)
    }
    throw error
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
  const [workspace, setWorkspace] = useState<WorkspaceState>(() => defaultWorkspace())
  const [runTarget, setRunTarget] = useState<RunTarget>('active')
  const [sessionMode, setSessionMode] = useState<SessionMode>('isolated')
  const [outputTab, setOutputTab] = useState<OutputTab>('result')
  const [resultText, setResultText] = useState('Ready. Choose a run target and execute Forge code.')
  const [stdoutText, setStdoutText] = useState('(no stdout yet)')
  const [lastRun, setLastRun] = useState('No runs yet')
  const [err, setErr] = useState(false)
  const [busy, setBusy] = useState(false)
  const [engine] = useState<'browser' | 'server'>(() =>
    typeof window !== 'undefined' && browserEngineAvailable() ? 'browser' : 'server'
  )

  const activeFile = useMemo(
    () => workspace.files.find((file) => file.id === workspace.activeFileId) ?? workspace.files[0],
    [workspace.activeFileId, workspace.files]
  )

  useEffect(() => {
    if (!runTarget.startsWith('file:')) return
    const fileId = runTarget.slice('file:'.length)
    if (workspace.files.some((file) => file.id === fileId)) return
    setRunTarget('active')
  }, [runTarget, workspace.files])

  const applyWorkspace = useCallback((update: (previous: WorkspaceState) => WorkspaceState) => {
    setWorkspace((previous) => normalizeWorkspace(update(previous)))
  }, [])

  const loadProject = useCallback((projectId: string) => {
    if (projectId === CUSTOM_PROJECT_ID) return
    const project = findPlaygroundProject(projectId)
    if (!project) return

    setWorkspace(instantiateProject(project))
    setRunTarget('active')
    setOutputTab('result')
    setErr(false)
    setResultText(`Loaded ${project.label}.`)
    setStdoutText('(no stdout yet)')
    setLastRun('No runs yet')
    window.ForgePlayground?.reset?.()
  }, [])

  const addFile = useCallback(() => {
    applyWorkspace((previous) => {
      const taken = previous.files.map((file) => file.name)
      const file: IdeFile = {
        id: nextFileId(),
        name: dedupeFileName('module.forge', taken),
        code: NEW_FILE_TEMPLATE,
      }
      return {
        ...previous,
        projectId: CUSTOM_PROJECT_ID,
        files: [...previous.files, file],
        activeFileId: file.id,
      }
    })
    setRunTarget('active')
  }, [applyWorkspace])

  const duplicateActiveFile = useCallback(() => {
    applyWorkspace((previous) => {
      const source = previous.files.find((file) => file.id === previous.activeFileId)
      if (!source) return previous

      const taken = previous.files.map((file) => file.name)
      const copy: IdeFile = {
        id: nextFileId(),
        name: dedupeFileName(copiedFileName(source.name), taken),
        code: source.code,
      }

      const index = previous.files.findIndex((file) => file.id === source.id)
      const files = [...previous.files]
      files.splice(index + 1, 0, copy)
      return {
        ...previous,
        projectId: CUSTOM_PROJECT_ID,
        files,
        activeFileId: copy.id,
      }
    })
    setRunTarget('active')
  }, [applyWorkspace])

  const renameFile = useCallback(
    (fileId: string) => {
      const target = workspace.files.find((file) => file.id === fileId)
      if (!target) return

      const nextName = window.prompt('Rename Forge file', target.name)
      if (nextName == null) return

      applyWorkspace((previous) => {
        const taken = previous.files.filter((file) => file.id !== fileId).map((file) => file.name)
        const normalizedName = dedupeFileName(ensureForgeFileName(nextName), taken)

        return {
          ...previous,
          projectId: CUSTOM_PROJECT_ID,
          files: previous.files.map((file) => (file.id === fileId ? { ...file, name: normalizedName } : file)),
        }
      })
    },
    [applyWorkspace, workspace.files]
  )

  const deleteFile = useCallback(
    (fileId: string) => {
      const target = workspace.files.find((file) => file.id === fileId)
      if (!target) return
      if (workspace.files.length <= 1) return

      const confirmed = window.confirm(`Delete ${target.name}?`)
      if (!confirmed) return

      applyWorkspace((previous) => {
        const index = previous.files.findIndex((file) => file.id === fileId)
        if (index < 0) return previous

        const files = previous.files.filter((file) => file.id !== fileId)
        const fallback = files[Math.max(0, index - 1)] ?? files[0]
        return {
          ...previous,
          projectId: CUSTOM_PROJECT_ID,
          files,
          activeFileId: previous.activeFileId === fileId ? fallback.id : previous.activeFileId,
          entryFileId: previous.entryFileId === fileId ? files[0].id : previous.entryFileId,
        }
      })
    },
    [applyWorkspace, workspace.files]
  )

  const moveFile = useCallback(
    (fileId: string, delta: -1 | 1) => {
      applyWorkspace((previous) => {
        const index = previous.files.findIndex((file) => file.id === fileId)
        const nextIndex = index + delta
        if (index < 0 || nextIndex < 0 || nextIndex >= previous.files.length) return previous

        const files = [...previous.files]
        const swap = files[index]
        files[index] = files[nextIndex]
        files[nextIndex] = swap

        return {
          ...previous,
          projectId: CUSTOM_PROJECT_ID,
          files,
        }
      })
    },
    [applyWorkspace]
  )

  const updateActiveFileCode = useCallback(
    (code: string) => {
      applyWorkspace((previous) => ({
        ...previous,
        projectId: CUSTOM_PROJECT_ID,
        files: previous.files.map((file) => (file.id === previous.activeFileId ? { ...file, code } : file)),
      }))
    },
    [applyWorkspace]
  )

  const resetSession = useCallback(async () => {
    setBusy(true)
    setErr(false)
    setOutputTab('result')
    try {
      if (browserEngineAvailable()) {
        window.ForgePlayground?.reset?.()
      } else {
        await resetServerSession()
      }
      setResultText('Session reset. Next run starts from a clean prelude environment.')
      setStdoutText('(stdout cleared)')
      setLastRun('Session reset')
    } catch (error) {
      setErr(true)
      setResultText(error instanceof Error ? error.message : String(error))
    } finally {
      setBusy(false)
    }
  }, [])

  const run = useCallback(
    async (targetOverride?: RunTarget) => {
      const target = targetOverride ?? runTarget
      const snippet = resolveRunSnippet(workspace, target)
      if (!snippet) {
        setErr(true)
        setResultText('Selected run target no longer exists.')
        return
      }

      setBusy(true)
      setErr(false)
      setOutputTab('result')
      setResultText(`Running ${snippet.label}...`)

      try {
        if (sessionMode === 'isolated') {
          if (browserEngineAvailable()) {
            window.ForgePlayground?.reset?.()
          } else {
            await resetServerSession()
          }
        }

        let data: RunResponse

        if (browserEngineAvailable()) {
          const raw = window.ForgePlayground!.eval(snippet.code)
          const parsed = parseRunJson(raw, 'ForgePlayground.eval')
          if (!parsed) throw new Error('Empty result from in-browser evaluator.')
          data = parsed
        } else {
          data = await runOnServer(snippet.code)
        }

        if (!data.ok) {
          setErr(true)
          setResultText(data.error)
          setStdoutText(data.printed?.trim() ? data.printed : '(no stdout)')
          setLastRun(`Failed · ${snippet.label}`)
          return
        }

        setResultText(formatResult(data))
        setStdoutText(data.printed?.trim() ? data.printed : '(no stdout)')
        setLastRun(`Success · ${snippet.label}`)
      } catch (error) {
        setErr(true)
        setResultText(error instanceof Error ? error.message : String(error))
      } finally {
        setBusy(false)
      }
    },
    [runTarget, sessionMode, workspace]
  )

  return (
    <div className="ide-page">
      <section className="ide-hero">
        <h1>Forge Web IDE</h1>
        <p>
          Build multi-file Forge workspaces, run active files or full projects, and inspect typed results and
          stdout side by side.
        </p>
        <p className="ide-hero-note">
          Engine: <strong>{engine === 'browser' ? 'In-browser (js_of_ocaml)' : 'Local API server'}</strong>. For
          setup details, see <Link to="/docs/install">Install docs</Link>.
        </p>
      </section>

      <section className="ide-shell" aria-label="Forge IDE workspace">
        <aside className="ide-sidebar">
          <div className="ide-control-group">
            <label htmlFor="ide-template-select">Starter workspace</label>
            <select
              id="ide-template-select"
              value={workspace.projectId}
              onChange={(event) => loadProject(event.target.value)}
            >
              {PLAYGROUND_PROJECTS.map((project) => (
                <option key={project.id} value={project.id} title={project.description}>
                  {project.label}
                </option>
              ))}
              {workspace.projectId === CUSTOM_PROJECT_ID ? <option value={CUSTOM_PROJECT_ID}>Custom workspace</option> : null}
            </select>
          </div>

          <div className="ide-control-group">
            <label htmlFor="ide-entry-select">Entry file</label>
            <select
              id="ide-entry-select"
              value={workspace.entryFileId}
              onChange={(event) => {
                const nextEntryFileId = event.target.value
                applyWorkspace((previous) => ({
                  ...previous,
                  projectId: CUSTOM_PROJECT_ID,
                  entryFileId: nextEntryFileId,
                }))
              }}
            >
              {workspace.files.map((file) => (
                <option key={file.id} value={file.id}>
                  {file.name}
                </option>
              ))}
            </select>
          </div>

          <div className="ide-sidebar-actions">
            <button type="button" className="btn" onClick={addFile}>
              + New file
            </button>
            <button type="button" className="btn" onClick={duplicateActiveFile} disabled={!activeFile}>
              Duplicate active
            </button>
          </div>

          <div className="ide-file-list-wrap">
            <div className="ide-file-list-header">Workspace files</div>
            <ul className="ide-file-list">
              {workspace.files.map((file, index) => (
                <li key={file.id} className={`ide-file-row${file.id === workspace.activeFileId ? ' active' : ''}`}>
                  <button
                    type="button"
                    className="ide-file-name"
                    onClick={() => applyWorkspace((previous) => ({ ...previous, activeFileId: file.id }))}
                  >
                    {file.name}
                  </button>
                  <div className="ide-file-actions">
                    <button
                      type="button"
                      className="icon-btn"
                      title="Run this file"
                      onClick={() => {
                        const target: RunTarget = `file:${file.id}`
                        setRunTarget(target)
                        void run(target)
                      }}
                    >
                      Run
                    </button>
                    <button
                      type="button"
                      className="icon-btn"
                      title="Rename file"
                      onClick={() => renameFile(file.id)}
                    >
                      Rename
                    </button>
                    <button
                      type="button"
                      className="icon-btn"
                      title="Move up"
                      onClick={() => moveFile(file.id, -1)}
                      disabled={index === 0}
                    >
                      Up
                    </button>
                    <button
                      type="button"
                      className="icon-btn"
                      title="Move down"
                      onClick={() => moveFile(file.id, 1)}
                      disabled={index === workspace.files.length - 1}
                    >
                      Down
                    </button>
                    <button
                      type="button"
                      className="icon-btn danger"
                      title="Delete file"
                      onClick={() => deleteFile(file.id)}
                      disabled={workspace.files.length <= 1}
                    >
                      Delete
                    </button>
                  </div>
                </li>
              ))}
            </ul>
          </div>
        </aside>

        <div className="ide-main">
          <div className="ide-toolbar">
            <label htmlFor="ide-run-target">
              Run target
              <select
                id="ide-run-target"
                value={runTarget}
                onChange={(event) => setRunTarget(event.target.value as RunTarget)}
              >
                <option value="active">Active file ({activeFile?.name ?? 'n/a'})</option>
                <option value="entry">
                  Entry file ({workspace.files.find((file) => file.id === workspace.entryFileId)?.name ?? 'n/a'})
                </option>
                <option value="workspace">Entire workspace ({workspace.files.length} files)</option>
                {workspace.files.map((file) => (
                  <option key={file.id} value={`file:${file.id}`}>
                    Only: {file.name}
                  </option>
                ))}
              </select>
            </label>

            <label htmlFor="ide-session-mode">
              Session
              <select
                id="ide-session-mode"
                value={sessionMode}
                onChange={(event) => setSessionMode(event.target.value as SessionMode)}
              >
                <option value="isolated">Isolated run (reset each run)</option>
                <option value="sticky">Sticky run (keep session state)</option>
              </select>
            </label>

            <button type="button" className="playground-run" disabled={busy} onClick={() => void run()}>
              {busy ? 'Running...' : 'Run target'}
            </button>
            <button type="button" className="btn" disabled={busy} onClick={() => void resetSession()}>
              Reset session
            </button>
          </div>

          <div className="ide-tab-strip" role="tablist" aria-label="Open files">
            {workspace.files.map((file) => (
              <button
                key={file.id}
                type="button"
                role="tab"
                aria-selected={file.id === workspace.activeFileId}
                className={`ide-tab${file.id === workspace.activeFileId ? ' active' : ''}`}
                onClick={() => applyWorkspace((previous) => ({ ...previous, activeFileId: file.id }))}
              >
                {file.name}
              </button>
            ))}
          </div>

          <div className="playground-editor-host ide-editor-host">
            <CodeMirror
              theme="none"
              basicSetup={false}
              value={activeFile?.code ?? ''}
              minHeight="320px"
              className="playground-editor"
              extensions={forgeExtensions}
              onChange={(value) => updateActiveFileCode(value)}
              editable
              indentWithTab
              spellCheck={false}
              aria-label="Forge source"
            />
          </div>

          <section className="ide-output" aria-label="Execution output">
            <div className="ide-output-header">
              <div className="ide-output-tabs" role="tablist" aria-label="Output views">
                <button
                  type="button"
                  role="tab"
                  aria-selected={outputTab === 'result'}
                  className={`ide-output-tab${outputTab === 'result' ? ' active' : ''}`}
                  onClick={() => setOutputTab('result')}
                >
                  Result
                </button>
                <button
                  type="button"
                  role="tab"
                  aria-selected={outputTab === 'stdout'}
                  className={`ide-output-tab${outputTab === 'stdout' ? ' active' : ''}`}
                  onClick={() => setOutputTab('stdout')}
                >
                  Stdout
                </button>
              </div>
              <div className="ide-run-meta">{lastRun}</div>
            </div>

            <div className={`playground-out${err && outputTab === 'result' ? ' err' : ''}`} aria-live="polite">
              {outputTab === 'result' ? (
                err ? <span className="err">{resultText}</span> : <span className="ok-type">{resultText}</span>
              ) : (
                <span>{stdoutText}</span>
              )}
            </div>
          </section>
        </div>
      </section>
    </div>
  )
}
