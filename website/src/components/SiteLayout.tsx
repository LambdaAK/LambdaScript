import { type PropsWithChildren } from 'react';
import { Link, useLocation } from 'react-router-dom';
import ThemeToggle from './ThemeToggle';

const GITHUB = 'https://github.com/LambdaAK/Forge';

// Top-bar links; a docs page lights up its own link if it has one, else "docs".
const links = [
  { to: '/docs/', label: 'docs' },
  { to: '/docs/quickstart/', label: 'quickstart' },
  { to: '/docs/examples/', label: 'examples' },
  { to: '/docs/how-it-works/', label: 'internals' },
];

function currentLink(path: string): string | undefined {
  const own = links.find((l) => l.to !== '/docs/' && path.startsWith(l.to));
  if (own) return own.to;
  return path.startsWith('/docs') ? '/docs/' : undefined;
}

function SiteLayout({ children }: PropsWithChildren) {
  const current = currentLink(useLocation().pathname);
  return (
    <>
      <a className="skip-link" href="#main">
        Skip to content
      </a>
      <header className="site-header">
        <div className="site-header-inner">
          <Link className="brand" to="/">
            <span className="spark" aria-hidden>
              ✻
            </span>
            forge
          </Link>
          <nav className="top-nav" aria-label="Pages">
            {links.map((l) => (
              <Link
                key={l.to}
                to={l.to}
                className={l.to === current ? 'active' : undefined}
                aria-current={l.to === current ? 'page' : undefined}
              >
                {l.label}
              </Link>
            ))}
            <a href={GITHUB} target="_blank" rel="noreferrer">
              github ↗
            </a>
          </nav>
          <ThemeToggle />
        </div>
      </header>

      <main id="main" className="site-main">
        {children}
      </main>

      <footer className="site-footer">
        <div className="site-footer-inner t-meta">
          <span>Forge</span>
          <a href={GITHUB} target="_blank" rel="noreferrer">
            source
          </a>
          <a href="https://alexkozik.com" target="_blank" rel="noreferrer">
            made by alex kozik
          </a>
        </div>
      </footer>
    </>
  );
}

export default SiteLayout;
