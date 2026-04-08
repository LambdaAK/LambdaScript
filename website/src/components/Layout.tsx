import { NavLink, Outlet } from 'react-router-dom'

function TopNavLink({ to, children }: { to: string; children: React.ReactNode }) {
  return (
    <NavLink to={to} end={to === '/'} className={({ isActive }) => `nav-link${isActive ? ' active' : ''}`}>
      {children}
    </NavLink>
  )
}

export function Layout() {
  return (
    <div className="layout">
      <header className="site-header">
        <div className="shell site-header-inner">
          <div className="brand-wrap">
            <NavLink to="/" className="brand-link" aria-label="Forge home">
              <span className="brand-mark">F</span>
              <span className="brand-name">Forge</span>
            </NavLink>
            <span className="brand-tagline">Typed FP, fast feedback, native output</span>
          </div>

          <nav className="site-nav" aria-label="Primary">
            <TopNavLink to="/">Home</TopNavLink>
            <TopNavLink to="/ide">IDE</TopNavLink>
            <TopNavLink to="/docs">Docs</TopNavLink>
          </nav>

          <div className="site-cta">
            <NavLink to="/ide" className="btn btn-primary btn-sm">
              Launch IDE
            </NavLink>
          </div>
        </div>
      </header>

      <main className="layout-main">
        <Outlet />
      </main>
    </div>
  )
}
