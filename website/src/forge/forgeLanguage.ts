/**
 * Forge syntax mode aligned with vscode-forge/syntaxes/forge.tmLanguage.json
 * (same keyword / storage / primitive / builtin splits; function names before `=`).
 */
import { StreamLanguage, type StringStream } from '@codemirror/language'

type ForgeState = {
  tokenize: (stream: StringStream, state: ForgeState) => string | null
  commentLevel: number
}

function kw(
  control: string[],
  other: string[],
  storage: string[],
  primitives: string[],
  builtins: string[]
): Record<string, string> {
  const m: Record<string, string> = {}
  for (const w of control) m[w] = 'controlKeyword'
  for (const w of other) m[w] = 'keyword'
  for (const w of storage) m[w] = 'definitionKeyword'
  for (const w of primitives) m[w] = 'typeName'
  for (const w of builtins) m[w] = 'variableName.standard'
  return m
}

const WORDS = kw(
  ['if', 'then', 'else', 'case', 'do', 'of', 'switch'],
  ['let', 'in', 'and', 'fn', 'for', 'with', 'end', 'where', 'requires', 'enum'],
  ['type', 'rec', 'trait', 'inter', 'impl', 'val'],
  [
    'int',
    'float',
    'bool',
    'string',
    'char',
    'unit',
    'Int',
    'Float',
    'Bool',
    'String',
    'Char',
    'Unit',
  ],
  [
    'print',
    'println',
    'int_to_str',
    'int_to_float',
    'float_to_int',
    'string_to_list',
    'map',
    'filter',
    'reduce_left',
    'reduce_right',
    'not',
  ]
)

/** Longest-first multi-character operators (see forge.tmLanguage.json operators). */
const MULTI_OPS = /^(?:\.\.\.|==|!=|<>|<=|>=|->|=>|<-|\|>|\|\||&&|::)/

function tokenString(stream: StringStream, state: ForgeState): string | null {
  let escaped = false
  let closed = false
  while (!stream.eol()) {
    const n = stream.next() as string | undefined
    if (n == null) break
    if (n === '"' && !escaped) {
      closed = true
      break
    }
    escaped = !escaped && n === '\\'
  }
  if (closed) state.tokenize = tokenBase
  return 'string'
}

function tokenComment(stream: StringStream, state: ForgeState): string | null {
  let prev = ''
  while (state.commentLevel > 0 && !stream.eol()) {
    const n = stream.next() as string | undefined
    if (n == null) break
    if (prev === '(' && n === '*') state.commentLevel++
    if (prev === '*' && n === ')') state.commentLevel--
    prev = n
  }
  if (state.commentLevel <= 0) state.tokenize = tokenBase
  return 'comment'
}

function tokenChar(stream: StringStream, state: ForgeState): string | null {
  while (!stream.eol()) {
    const n = stream.next() as string | undefined
    if (n == null) break
    if (n === '\\') {
      stream.next()
      continue
    }
    if (n === "'") {
      state.tokenize = tokenBase
      break
    }
  }
  return 'string'
}

function tokenBase(stream: StringStream, state: ForgeState): string | null {
  if (stream.match(/^\/\//)) {
    stream.skipToEnd()
    return 'comment'
  }

  if (stream.match(/^\(\*/)) {
    state.commentLevel = 1
    state.tokenize = tokenComment
    return tokenComment(stream, state)
  }

  if (stream.eat('"')) {
    state.tokenize = tokenString
    return tokenString(stream, state)
  }

  if (stream.match(/^-?\d+(?:\.\d+)?\b/)) return 'number'

  if (stream.match(MULTI_OPS)) return 'operator'

  const p = stream.peek()
  if (p === '(' || p === ')' || p === '[' || p === ']' || p === '{' || p === '}' || p === ',' || p === ';') {
    stream.next()
    return 'punctuation'
  }

  if (stream.match(/^[+\-*/%<>=%:.!]/)) return 'operator'

  if (stream.match(/^\|/)) return 'operator'

  if (stream.match(/^[A-Z][a-zA-Z0-9_']*/)) return 'typeName'

  if (stream.peek() === "'") {
    if (stream.match(/^'[a-z][a-zA-Z0-9_]*/, false)) {
      stream.match(/^'[a-z][a-zA-Z0-9_]*/)
      return 'typeName'
    }
    stream.next()
    state.tokenize = tokenChar
    return tokenChar(stream, state)
  }

  if (stream.match(/^[a-z_][a-zA-Z0-9_']*/)) {
    const w = stream.current()
    if (w === 'true' || w === 'false') return 'bool'
    if (WORDS[w]) return WORDS[w]
    if (stream.match(/^\s*=/, false)) return 'variableName.definition'
    return 'variableName'
  }

  if (stream.eol()) return null

  stream.next()
  return null
}

const forgeParser = {
  name: 'forge',
  startState(): ForgeState {
    return { tokenize: tokenBase, commentLevel: 0 }
  },
  token(stream: StringStream, state: ForgeState): string | null {
    if (stream.eatSpace()) return null
    return state.tokenize(stream, state)
  },
  languageData: {
    commentTokens: { line: '//', block: { open: '(*', close: '*)' } },
    autocomplete: Array.from(
      new Set([
        ...Object.keys(WORDS),
        'true',
        'false',
        'if',
        'then',
        'else',
        'case',
        'let',
        'in',
        'fn',
        'type',
        'trait',
        'impl',
        'val',
      ])
    ),
  },
}

export const forgeLanguage = StreamLanguage.define(forgeParser)
