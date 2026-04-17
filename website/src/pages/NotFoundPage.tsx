import { Link } from 'react-router-dom';

function NotFoundPage() {
  return (
    <section className="not-found reveal">
      <p className="section-kicker">404</p>
      <h1>Page not found.</h1>
      <p>
        This route does not exist in the Forge docs website.
      </p>
      <div className="cta-row">
        <Link className="btn btn-primary" to="/">
          Back home
        </Link>
        <Link className="btn" to="/docs/">
          Open docs
        </Link>
      </div>
    </section>
  );
}

export default NotFoundPage;
