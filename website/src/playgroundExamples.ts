export type PlaygroundProjectFile = {
  name: string
  code: string
}

export type PlaygroundProject = {
  id: string
  label: string
  description: string
  files: PlaygroundProjectFile[]
  /** Name of the default entry file for quick runs. */
  entryFile: string
}

export const PLAYGROUND_PROJECTS: PlaygroundProject[] = [
  {
    id: 'quickstart',
    label: 'Quickstart App',
    description: 'Small entry file with helper functions and straightforward output.',
    entryFile: 'main.forge',
    files: [
      {
        name: 'math.forge',
        code: `let add_one x = x + 1
let clamp low high x = if x < low then low else if x > high then high else x`,
      },
      {
        name: 'main.forge',
        code: `let score = clamp 0 100 (add_one 41)
let _ = println ("score=" ^ int_to_str score)`,
      },
    ],
  },
  {
    id: 'adt-engine',
    label: 'ADT + Pattern Matching',
    description: 'Split tree definitions from app logic and run together as one workspace.',
    entryFile: 'app.forge',
    files: [
      {
        name: 'tree.forge',
        code: `type rec Tree =
  | Leaf of Int
  | Node of (Tree, Tree)

let rec sum_tree t =
  case t do
  | Leaf n -> n
  | Node (left, right) -> sum_tree left + sum_tree right`,
      },
      {
        name: 'app.forge',
        code: `let value = sum_tree (Node (Leaf 4, Node (Leaf 8, Leaf 15)))
let _ = println ("tree=" ^ int_to_str value)`,
      },
    ],
  },
  {
    id: 'traits-lab',
    label: 'Traits Lab',
    description: 'Experiment with trait impls in one file and app code in another.',
    entryFile: 'main.forge',
    files: [
      {
        name: 'traits.forge',
        code: `impl Semigroup for Bool where
  let mappend x y = x
  let (++) x y = mappend x y
end`,
      },
      {
        name: 'main.forge',
        code: `let _ = println (true ++ false)`,
      },
    ],
  },
  {
    id: 'lists-recursion',
    label: 'Lists + Recursion',
    description: 'Build helpers and entry logic separately for iteration workflows.',
    entryFile: 'main.forge',
    files: [
      {
        name: 'collections.forge',
        code: `let rec sum lst =
  case lst do
  | [] -> 0
  | head :: tail -> head + sum tail

let rec len lst =
  case lst do
  | [] -> 0
  | _ :: tail -> 1 + len tail`,
      },
      {
        name: 'main.forge',
        code: `let nums = [2, 4, 8, 16]
let _ = println ("sum=" ^ int_to_str (sum nums))
let _ = println ("len=" ^ int_to_str (len nums))`,
      },
    ],
  },
  {
    id: 'blank',
    label: 'Blank Workspace',
    description: 'Start from scratch with a single entry file.',
    entryFile: 'main.forge',
    files: [
      {
        name: 'main.forge',
        code: 'let _ = println "Hello from Forge"',
      },
    ],
  },
]

export const DEFAULT_PLAYGROUND_PROJECT_ID = PLAYGROUND_PROJECTS[0].id

export function findPlaygroundProject(id: string): PlaygroundProject | undefined {
  return PLAYGROUND_PROJECTS.find((project) => project.id === id)
}
