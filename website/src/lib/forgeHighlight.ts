export type ForgeTokenKind =
  | 'comment'
  | 'string'
  | 'number'
  | 'keyword'
  | 'type'
  | 'builtin'
  | 'macro'
  | 'operator'
  | 'punctuation'
  | 'identifier';

export type ForgeToken = {
  text: string;
  kind: ForgeTokenKind | null;
};

const KEYWORDS = new Set([
  'let',
  'rec',
  'fn',
  'fun',
  'match',
  'with',
  'case',
  'do',
  'if',
  'then',
  'else',
  'in',
  'type',
  'trait',
  'inter',
  'impl',
  'for',
  'where',
  'requires',
  'end',
  'mod',
  'import',
  'use',
  'val',
  'and',
]);

const TYPE_NAMES = new Set(['int', 'float', 'bool', 'string', 'char', 'unit']);

const BUILTIN_NAMES = new Set([
  'print',
  'println',
  'int_to_str',
  'int_to_float',
  'float_to_int',
  'str_length',
  'str_concat',
  'str_slice',
  'list_length',
  'list_head',
  'list_tail',
  'list_nth',
  'map',
  'filter',
  'reduce_left',
  'reduce_right',
  'stringify',
  'concat',
  'concat_str',
  'count_args',
  'vec',
]);

const MULTI_OPERATORS = ['->', '=>', '::', '==', '!=', '<=', '>=', '&&', '||'];

const OPERATOR_CHARS = new Set(['+', '-', '*', '/', '%', '=', '<', '>', '|', '&', ':', '.', '!']);

const PUNCTUATION_CHARS = new Set(['(', ')', '[', ']', '{', '}', ',', ';']);

function isWhitespace(ch: string): boolean {
  return ch === ' ' || ch === '\n' || ch === '\t' || ch === '\r' || ch === '\f';
}

function isDigit(ch: string): boolean {
  return ch >= '0' && ch <= '9';
}

function isIdentifierStart(ch: string): boolean {
  return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch === '_';
}

function isIdentifierPart(ch: string): boolean {
  return isIdentifierStart(ch) || isDigit(ch) || ch === '\'' || ch === '_';
}

function classifyIdentifier(text: string): ForgeTokenKind {
  if (text.endsWith('!') || text === 'macro_rules') {
    return 'macro';
  }
  if (KEYWORDS.has(text)) {
    return 'keyword';
  }
  if (TYPE_NAMES.has(text)) {
    return 'type';
  }
  if (BUILTIN_NAMES.has(text)) {
    return 'builtin';
  }
  return 'identifier';
}

export function highlightForge(code: string): ForgeToken[] {
  const tokens: ForgeToken[] = [];
  const length = code.length;
  let i = 0;

  while (i < length) {
    const ch = code[i];
    const next = i + 1 < length ? code[i + 1] : '';

    if (isWhitespace(ch)) {
      let j = i + 1;
      while (j < length && isWhitespace(code[j])) {
        j += 1;
      }
      tokens.push({ text: code.slice(i, j), kind: null });
      i = j;
      continue;
    }

    if (ch === '/' && next === '/') {
      let j = i + 2;
      while (j < length && code[j] !== '\n') {
        j += 1;
      }
      tokens.push({ text: code.slice(i, j), kind: 'comment' });
      i = j;
      continue;
    }

    if (ch === '"' || ch === '\'') {
      const quote = ch;
      let j = i + 1;
      while (j < length) {
        if (code[j] === '\\') {
          j += 2;
          continue;
        }
        if (code[j] === quote) {
          j += 1;
          break;
        }
        j += 1;
      }
      tokens.push({ text: code.slice(i, j), kind: 'string' });
      i = j;
      continue;
    }

    if (ch === '$') {
      let j = i + 1;
      while (j < length && isIdentifierPart(code[j])) {
        j += 1;
      }
      if (j === i + 1 && j < length && (code[j] === '(' || code[j] === '[' || code[j] === '{')) {
        j += 1;
      }
      tokens.push({ text: code.slice(i, j), kind: 'macro' });
      i = j;
      continue;
    }

    if (isDigit(ch)) {
      let j = i + 1;
      while (j < length && (isDigit(code[j]) || code[j] === '_')) {
        j += 1;
      }
      if (j < length && code[j] === '.' && j + 1 < length && isDigit(code[j + 1])) {
        j += 2;
        while (j < length && (isDigit(code[j]) || code[j] === '_')) {
          j += 1;
        }
      }
      tokens.push({ text: code.slice(i, j), kind: 'number' });
      i = j;
      continue;
    }

    if (isIdentifierStart(ch)) {
      let j = i + 1;
      while (j < length && isIdentifierPart(code[j])) {
        j += 1;
      }
      if (j < length && code[j] === '!') {
        j += 1;
      }
      const text = code.slice(i, j);
      tokens.push({ text, kind: classifyIdentifier(text) });
      i = j;
      continue;
    }

    const multiOperator = MULTI_OPERATORS.find((operator) => code.startsWith(operator, i));
    if (multiOperator) {
      tokens.push({ text: multiOperator, kind: 'operator' });
      i += multiOperator.length;
      continue;
    }

    if (OPERATOR_CHARS.has(ch)) {
      tokens.push({ text: ch, kind: 'operator' });
      i += 1;
      continue;
    }

    if (PUNCTUATION_CHARS.has(ch)) {
      tokens.push({ text: ch, kind: 'punctuation' });
      i += 1;
      continue;
    }

    tokens.push({ text: ch, kind: null });
    i += 1;
  }

  return tokens;
}
