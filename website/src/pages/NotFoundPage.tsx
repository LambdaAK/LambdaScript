import { Link } from 'react-router-dom';

function NotFoundPage() {
  return (
    <section className="not-found">
      <h1 className="page-title">Page not found</h1>
      <p className="t-sub">There is nothing at this address.</p>
      <p className="t-sub link-row">
        <Link className="ink" to="/">
          home
        </Link>
        <Link className="ink" to="/docs/">
          docs
        </Link>
      </p>
    </section>
  );
}

export default NotFoundPage;
