import { NavLink, Outlet } from 'react-router-dom'

export function Layout() {
  return (
    <div className="layout">
      <header className="layout-header">
        <NavLink to="/" className="brand">
          Forge
        </NavLink>
        <nav className="nav-links">
          <NavLink to="/" end className={({ isActive }) => (isActive ? 'active' : undefined)}>
            Home
          </NavLink>
          <NavLink to="/playground" className={({ isActive }) => (isActive ? 'active' : undefined)}>
            Playground
          </NavLink>
          <NavLink to="/docs" className={({ isActive }) => (isActive ? 'active' : undefined)}>
            Docs
          </NavLink>
        </nav>
      </header>
      <main className="layout-main">
        <Outlet />
      </main>
    </div>
  )
}
