import { BrowserRouter, Navigate, Route, Routes } from 'react-router-dom'
import { Layout } from './components/Layout'
import { DocsLayout } from './components/DocsLayout'
import HomePage from './pages/HomePage'
import PlaygroundPage from './pages/PlaygroundPage'
import { DocsMarkdownPage } from './pages/docs/DocsMarkdownPage'

export default function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route element={<Layout />}>
          <Route index element={<HomePage />} />
          <Route path="playground" element={<PlaygroundPage />} />
          <Route path="docs" element={<DocsLayout />}>
            <Route index element={<Navigate to="overview" replace />} />
            <Route path="overview" element={<DocsMarkdownPage slug="overview" />} />
            <Route path="install" element={<DocsMarkdownPage slug="install" />} />
            <Route path="types" element={<DocsMarkdownPage slug="types" />} />
            <Route path="expressions" element={<DocsMarkdownPage slug="expressions" />} />
            <Route path="pattern-matching" element={<DocsMarkdownPage slug="pattern-matching" />} />
            <Route path="traits" element={<DocsMarkdownPage slug="traits" />} />
            <Route path="builtins" element={<DocsMarkdownPage slug="builtins" />} />
            <Route path="prelude" element={<DocsMarkdownPage slug="prelude" />} />
          </Route>
          <Route path="*" element={<Navigate to="/" replace />} />
        </Route>
      </Routes>
    </BrowserRouter>
  )
}
