import { mkdirSync } from "node:fs";
import { join } from "node:path";
import { chromium } from "playwright";

const root = new URL("..", import.meta.url).pathname;
const outDir = join(root, ".local", "screenshots");
const url = process.env.APP_URL || "http://127.0.0.1:4174/";

mkdirSync(outDir, { recursive: true });

const browser = await chromium.launch({ headless: true });
const page = await browser.newPage({ viewport: { width: 1440, height: 980 } });
await page.goto(url, { waitUntil: "networkidle" });
await page.screenshot({ path: join(outDir, "desktop.png"), fullPage: true });

await page.setViewportSize({ width: 390, height: 844 });
await page.goto(url, { waitUntil: "networkidle" });
await page.screenshot({ path: join(outDir, "mobile.png"), fullPage: true });

const title = await page.locator("h1").textContent();
if (title !== "Signal Gym") {
  throw new Error(`Unexpected app title: ${title}`);
}

await browser.close();
console.log(`screenshots written to ${outDir}`);
