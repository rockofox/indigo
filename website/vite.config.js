import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';
import { copyFileSync } from 'fs';
import { join } from 'path';

// Plugin to copy built index.html to 404.html for GitHub Pages SPA routing
const copy404Plugin = () => ({
  name: 'copy-404',
  closeBundle() {
    const src = join(process.cwd(), 'dist', 'index.html');
    const dest = join(process.cwd(), 'dist', '404.html');
    try {
      copyFileSync(src, dest);
    } catch (err) {
      console.warn('Failed to copy index.html to 404.html:', err);
    }
  },
});

export default defineConfig({
  plugins: [elmPlugin(), copy404Plugin()],
  // base: '/indigo/', // GitHub Pages base path (repo name)
});
