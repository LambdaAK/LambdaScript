# Forge Website (React + TypeScript + Vite)

This is the website/docs app for Forge.

## Local development

From repository root:

```bash
npm --prefix website install
npm --prefix website run dev
```

Then open the URL shown by Vite (typically `http://localhost:5173`).

## Production build

```bash
npm --prefix website run build
npm --prefix website run preview
```

## Netlify

`netlify.toml` at repository root is configured to:

- run `npm ci --prefix website && npm --prefix website run build`
- publish `website/dist`
- rewrite SPA routes to `index.html`
- use Node.js `20`

If you configure settings manually in Netlify:

- **Build command**: `npm ci --prefix website && npm --prefix website run build`
- **Publish directory**: `website/dist`
- **Base directory**: leave empty

SEO files are generated from static assets in `website/public/`:

- `robots.txt`
- `sitemap.xml`
- `og-image.svg`

If your production domain is not `forge-lang.netlify.app`, update the URLs in
`robots.txt` and `sitemap.xml` before deploying.
