import { Navigate, Route, Routes } from 'react-router-dom';
import SiteLayout from './components/SiteLayout';
import HomePage from './pages/HomePage';
import DocsOverviewPage from './pages/DocsOverviewPage';
import QuickstartPage from './pages/QuickstartPage';
import LanguageFeaturesPage from './pages/LanguageFeaturesPage';
import StandardLibraryPage from './pages/StandardLibraryPage';
import ExamplesPage from './pages/ExamplesPage';
import GrammarPage from './pages/GrammarPage';
import HowItWorksPage from './pages/HowItWorksPage';
import WhyForgePage from './pages/WhyForgePage';

function App() {
  return (
    <SiteLayout>
      <Routes>
        <Route path="/" element={<HomePage />} />
        <Route path="/docs" element={<Navigate to="/docs/" replace />} />
        <Route path="/docs/" element={<DocsOverviewPage />} />
        <Route path="/docs/quickstart/" element={<QuickstartPage />} />
        <Route
          path="/docs/language-features/"
          element={<LanguageFeaturesPage />}
        />
        <Route
          path="/docs/standard-library/"
          element={<StandardLibraryPage />}
        />
        <Route path="/docs/examples/" element={<ExamplesPage />} />
        <Route path="/docs/grammar/" element={<GrammarPage />} />
        <Route path="/docs/how-it-works/" element={<HowItWorksPage />} />
        <Route path="/docs/why-forge/" element={<WhyForgePage />} />
        <Route path="*" element={<Navigate to="/" replace />} />
      </Routes>
    </SiteLayout>
  );
}

export default App;
