import { type PropsWithChildren } from 'react';
import { Link, NavLink } from 'react-router-dom';
import forgeLogo from '../../Forge.png';

function SiteLayout({ children }: PropsWithChildren) {
  return (
    <div className="site-shell">
      <header className="site-header">
        <div className="container site-header-inner">
          <NavLink className="brand" to="/">
            <img
              src={forgeLogo}
              alt=""
              width={28}
              height={28}
              className="brand-logo"
              decoding="async"
            />
            <span className="brand-name">Forge</span>
          </NavLink>
          <nav className="top-nav" aria-label="Primary">
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
            <a
              href="https://github.com/LambdaAK/Forge"
              target="_blank"
              rel="noreferrer"
            >
              GitHub
            </a>
          </nav>
          <Link className="btn btn-header" to="/docs/quickstart/">
            Get started
          </Link>
        </div>
      </header>

      <main className="container site-main">{children}</main>

      <footer className="site-footer">
        <div className="container site-footer-inner">
          <NavLink className="brand brand-footer" to="/">
            <span className="brand-mark" aria-hidden>
              F
            </span>
            <span className="brand-name">Forge</span>
          </NavLink>
          <nav className="footer-nav" aria-label="Footer">
            <NavLink to="/docs/">Docs</NavLink>
            <NavLink to="/docs/examples/">Examples</NavLink>
            <NavLink to="/docs/how-it-works/">How it works</NavLink>
            <a
              href="https://github.com/LambdaAK/Forge"
              target="_blank"
              rel="noreferrer"
            >
              GitHub
            </a>
          </nav>
          <span className="footer-year">Forge · {new Date().getFullYear()}</span>
        </div>
      </footer>
    </div>
  );
}

export default SiteLayout;
