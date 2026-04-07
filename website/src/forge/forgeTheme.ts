/**
 * Colors aligned with VS Code Dark+ defaults for TextMate scopes used in
 * vscode-forge/syntaxes/forge.tmLanguage.json
 */
import { HighlightStyle, syntaxHighlighting } from '@codemirror/language'
import { EditorView } from '@codemirror/view'
import { tags } from '@lezer/highlight'

/** Dark+–style token colors (same roles as VS Code for Forge scopes). */
const forgeHighlightStyle = HighlightStyle.define([
  { tag: tags.comment, color: '#6a9955' },
  { tag: tags.controlKeyword, color: '#c586c0' },
  { tag: tags.keyword, color: '#569cd6' },
  { tag: tags.definitionKeyword, color: '#569cd6' },
  { tag: tags.typeName, color: '#4ec9b0' },
  { tag: tags.string, color: '#ce9178' },
  { tag: tags.number, color: '#b5cea8' },
  { tag: tags.bool, color: '#569cd6' },
  { tag: tags.variableName, color: '#9cdcfe' },
  { tag: tags.definition(tags.variableName), color: '#dcdcaa' },
  { tag: tags.standard(tags.variableName), color: '#dcdcaa' },
  { tag: tags.operator, color: '#d4d4d4' },
  { tag: tags.punctuation, color: '#d4d4d4' },
  { tag: tags.bracket, color: '#d4d4d4' },
])

export const forgeSyntaxHighlighting = syntaxHighlighting(forgeHighlightStyle, { fallback: true })

/** Chrome to match .playground-editor (index.css) + selection/cursor. */
export const forgeEditorChrome = EditorView.theme(
  {
    '&': {
      fontSize: '0.88rem',
      lineHeight: '1.45',
      backgroundColor: '#121a24',
      color: '#d4d4d4',
      borderRadius: '8px',
      border: '1px solid #263244',
    },
    '&.cm-editor.cm-focused': {
      outline: 'none',
    },
    '.cm-scroller': {
      fontFamily: 'ui-monospace, Menlo, monospace',
      minHeight: '220px',
      overflow: 'auto',
    },
    '.cm-content': {
      padding: '0.75rem',
      caretColor: '#e8eaed',
    },
    '.cm-line': {
      padding: 0,
    },
    '.cm-selectionBackground, ::selection': {
      backgroundColor: '#264f78 !important',
    },
    '&.cm-focused .cm-cursor': {
      borderLeftColor: '#e8eaed',
    },
    '.cm-activeLine': {
      backgroundColor: 'transparent',
    },
  },
  { dark: true }
)
