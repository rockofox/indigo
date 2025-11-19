import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
  plugins: [elmPlugin()],
  // base: '/indigo/', // GitHub Pages base path (repo name)
});
