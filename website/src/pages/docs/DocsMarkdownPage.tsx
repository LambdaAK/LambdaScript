import { MarkdownArticle } from '../../components/MarkdownArticle'
import overview from '../../content/docs/overview.md?raw'
import install from '../../content/docs/install.md?raw'
import types from '../../content/docs/types.md?raw'
import expressions from '../../content/docs/expressions.md?raw'
import patternMatching from '../../content/docs/pattern-matching.md?raw'
import traits from '../../content/docs/traits.md?raw'
import builtins from '../../content/docs/builtins.md?raw'
import prelude from '../../content/docs/prelude.md?raw'

const pages = {
  overview,
  install,
  types,
  expressions,
  'pattern-matching': patternMatching,
  traits,
  builtins,
  prelude,
} as const

export type DocSlug = keyof typeof pages

type Props = { slug: DocSlug }

export function DocsMarkdownPage({ slug }: Props) {
  return <MarkdownArticle source={pages[slug]} />
}
