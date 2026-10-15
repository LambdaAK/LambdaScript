import { type PropsWithChildren } from 'react';
import { NavLink } from 'react-router-dom';

const pages = [
  { to: '/docs/', label: 'overview', end: true },
  { to: '/docs/quickstart/', label: 'quickstart' },
  { to: '/docs/language-features/', label: 'language' },
  { to: '/docs/standard-library/', label: 'standard library' },
  { to: '/docs/examples/', label: 'examples' },
  { to: '/docs/grammar/', label: 'grammar' },
  { to: '/docs/how-it-works/', label: 'internals' },
  { to: '/docs/vscode-extension/', label: 'vs code' },
];

function DocsLayout({ children }: PropsWithChildren) {
  return (
    <>
      <nav className="docs-nav" aria-label="Documentation">
        {pages.map((p) => (
          <NavLink
            key={p.to}
            to={p.to}
            end={p.end}
            className={({ isActive }) => (isActive ? 'active' : undefined)}
          >
            {p.label}
          </NavLink>
        ))}
      </nav>
      <article className="docs-content">{children}</article>
    </>
  );
}

export default DocsLayout;
