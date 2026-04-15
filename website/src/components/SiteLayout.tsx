import { type PropsWithChildren } from 'react';
import { NavLink } from 'react-router-dom';

function SiteLayout({ children }: PropsWithChildren) {
  return (
    <>
      <header className="site-header">
        <div className="container site-header-inner">
          <NavLink className="brand" to="/">
            <span>Forge</span>
            <small>LambdaScript Repository</small>
          </NavLink>
          <nav className="top-nav" aria-label="Primary">
            <NavLink
              to="/"
              end
              className={({ isActive }) => (isActive ? 'active' : undefined)}
            >
              Home
            </NavLink>
            <NavLink
              to="/docs/"
              className={({ isActive }) => (isActive ? 'active' : undefined)}
            >
              Docs
            </NavLink>
            <NavLink
              to="/docs/examples/"
              className={({ isActive }) => (isActive ? 'active' : undefined)}
            >
              Examples
            </NavLink>
            <NavLink
              to="/docs/quickstart/"
              className={({ isActive }) => (isActive ? 'active' : undefined)}
            >
              Quickstart
            </NavLink>
            <a
              className="cta"
              href="https://github.com/LambdaAK/LambdaScript"
              target="_blank"
              rel="noreferrer"
            >
              GitHub
            </a>
          </nav>
        </div>
      </header>

      <main className="container">{children}</main>

      <footer className="container">
        <span>
          Forge / LambdaScript • <span>{new Date().getFullYear()}</span>
        </span>
      </footer>
    </>
  );
}

export default SiteLayout;
