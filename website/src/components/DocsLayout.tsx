import { NavLink, Outlet } from 'react-router-dom'

function SideLink({ to, children }: { to: string; children: React.ReactNode }) {
  return (
    <NavLink to={to} className={({ isActive }) => (isActive ? 'active' : undefined)}>
      {children}
    </NavLink>
  )
}

export function DocsLayout() {
  return (
    <div className="docs-shell">
      <aside className="docs-nav">
        <div className="docs-nav-section">Start</div>
        <SideLink to="/docs/overview">Overview</SideLink>
        <SideLink to="/docs/install">Install</SideLink>

        <div className="docs-nav-section">Language</div>
        <SideLink to="/docs/types">Types</SideLink>
        <SideLink to="/docs/expressions">Expressions</SideLink>
        <SideLink to="/docs/pattern-matching">Pattern matching</SideLink>
        <SideLink to="/docs/traits">Traits</SideLink>

        <div className="docs-nav-section">Reference</div>
        <SideLink to="/docs/builtins">Built-ins</SideLink>
        <SideLink to="/docs/prelude">Standard prelude</SideLink>
      </aside>
      <div>
        <Outlet />
      </div>
    </div>
  )
}
