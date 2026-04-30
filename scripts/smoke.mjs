import { existsSync, readFileSync, readdirSync } from "node:fs";
import { join } from "node:path";

const root = new URL("..", import.meta.url).pathname;
const required = [
  "dist/index.html",
  "dist/app.js",
  "dist/style.css",
  "dist/mark.svg",
  "README.md",
  "STATUS.md",
  "PUBLICATION_AUDIT.md"
];

for (const relative of required) {
  const path = join(root, relative);
  if (!existsSync(path)) {
    throw new Error(`Missing required artifact: ${relative}`);
  }
}

const publicTextFiles = [
  "README.md",
  "STATUS.md",
  "PUBLICATION_AUDIT.md",
  "docs/research-notes.md",
  "docs/design.md",
  "src/SignalGym/App.purs",
  "src/SignalGym/Training.purs",
  "src/SignalGym/Storage.js"
];

const forbidden = [
  /sk-[A-Za-z0-9_-]{12,}/,
  /gho_[A-Za-z0-9_]+/,
  /ghp_[A-Za-z0-9_]+/,
  /BEGIN (RSA|OPENSSH|EC|PRIVATE) KEY/,
  /\/home\/masaya\/\.config/
];

for (const relative of publicTextFiles) {
  const text = readFileSync(join(root, relative), "utf8");
  for (const pattern of forbidden) {
    if (pattern.test(text)) {
      throw new Error(`Public-safety pattern found in ${relative}: ${pattern}`);
    }
  }
}

const distFiles = readdirSync(join(root, "dist"));
if (!distFiles.includes("app.js") || !distFiles.includes("index.html")) {
  throw new Error("dist is not deployable");
}

console.log("smoke ok");
