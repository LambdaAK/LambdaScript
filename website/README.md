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

- run website build
- publish `website/dist`
- rewrite SPA routes to `index.html`
