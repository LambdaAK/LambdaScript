import { type PropsWithChildren } from 'react';
import { NavLink } from 'react-router-dom';

function navClass({ isActive }: { isActive: boolean }) {
  return isActive ? 'active' : undefined;
}

function DocsLayout({ children }: PropsWithChildren) {
  return (
    <div className="docs-grid">
      <aside className="side-nav">
        <h3>Documentation</h3>
        <NavLink to="/docs/" end className={navClass}>
          Overview
        </NavLink>
        <NavLink to="/docs/quickstart/" className={navClass}>
          Quickstart
        </NavLink>
        <NavLink to="/docs/language-features/" className={navClass}>
          Language Features
        </NavLink>
        <NavLink to="/docs/standard-library/" className={navClass}>
          Standard Library
        </NavLink>
        <NavLink to="/docs/examples/" className={navClass}>
          Examples
        </NavLink>
        <NavLink to="/docs/grammar/" className={navClass}>
          Grammar
        </NavLink>
        <NavLink to="/docs/how-it-works/" className={navClass}>
          How It Works
        </NavLink>
      </aside>

      <article className="docs-content">{children}</article>
    </div>
  );
}

export default DocsLayout;
