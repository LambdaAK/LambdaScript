import { Link } from 'react-router-dom'

export default function HomePage() {
  return (
    <div className="home-hero">
      <h1>Forge</h1>
      <p className="lede">
        A statically typed functional language with inference, ADTs, pattern matching, and a rich
        prelude of traits and data types—implemented by an interpreter and a native compiler in this
        repository.
      </p>
      <div className="home-actions">
        <Link to="/playground" className="btn btn-primary">
          Playground
        </Link>
        <Link to="/docs" className="btn">
          Documentation
        </Link>
      </div>
      <p className="playground-note">
        The Playground uses the same evaluator as the REPL (including the standard prelude). With the
        js_of_ocaml bundle in place (see <Link to="/docs/install">Install</Link>), it runs entirely in
        the browser; otherwise you can use the optional local API.
      </p>
    </div>
  )
}
