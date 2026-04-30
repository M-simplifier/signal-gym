import { copyFileSync, mkdirSync, readdirSync, statSync } from "node:fs";
import { join } from "node:path";

const root = new URL("..", import.meta.url).pathname;
const dist = join(root, "dist");
const publicDir = join(root, "public");

mkdirSync(dist, { recursive: true });
copyFileSync(join(root, "index.html"), join(dist, "index.html"));
copyFileSync(join(root, "style.css"), join(dist, "style.css"));

for (const entry of readdirSync(publicDir)) {
  const source = join(publicDir, entry);
  if (statSync(source).isFile()) {
    copyFileSync(source, join(dist, entry));
  }
}
