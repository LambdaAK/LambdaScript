import { HighlightStyle, syntaxHighlighting } from '@codemirror/language'
import { EditorView } from '@codemirror/view'
import { tags } from '@lezer/highlight'

const forgeHighlightStyle = HighlightStyle.define([
  { tag: tags.comment, color: '#5d7f6f' },
  { tag: tags.controlKeyword, color: '#f4b16a' },
  { tag: tags.keyword, color: '#81c8ff' },
  { tag: tags.definitionKeyword, color: '#8fd0ff' },
  { tag: tags.typeName, color: '#7ee0c2' },
  { tag: tags.string, color: '#f3a57d' },
  { tag: tags.number, color: '#c8f29e' },
  { tag: tags.bool, color: '#f7d774' },
  { tag: tags.variableName, color: '#c9dcff' },
  { tag: tags.definition(tags.variableName), color: '#fff0a8' },
  { tag: tags.standard(tags.variableName), color: '#ffe08f' },
  { tag: tags.operator, color: '#d6e0ef' },
  { tag: tags.punctuation, color: '#d6e0ef' },
  { tag: tags.bracket, color: '#d6e0ef' },
])

export const forgeSyntaxHighlighting = syntaxHighlighting(forgeHighlightStyle, { fallback: true })

export const forgeEditorChrome = EditorView.theme(
  {
    '&': {
      fontSize: '0.9rem',
      lineHeight: '1.5',
      backgroundColor: '#0f1728',
      color: '#e5edf8',
      borderRadius: '12px',
      border: '1px solid #253a56',
    },
    '&.cm-editor.cm-focused': {
      outline: 'none',
      borderColor: '#3f7fc2',
      boxShadow: '0 0 0 2px rgba(63, 127, 194, 0.25)',
    },
    '.cm-scroller': {
      fontFamily: "'IBM Plex Mono', 'JetBrains Mono', ui-monospace, SFMono-Regular, Menlo, monospace",
      minHeight: '320px',
      overflow: 'auto',
    },
    '.cm-content': {
      padding: '0.9rem',
      caretColor: '#f8fbff',
    },
    '.cm-line': {
      padding: 0,
    },
    '.cm-selectionBackground, ::selection': {
      backgroundColor: '#21496f !important',
    },
    '&.cm-focused .cm-cursor': {
      borderLeftColor: '#f8fbff',
    },
    '.cm-activeLine': {
      backgroundColor: 'rgba(47, 82, 120, 0.2)',
    },
  },
  { dark: true }
)
