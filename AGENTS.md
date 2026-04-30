# Agent Guide

This file is public.

Use this order before making substantial changes:

1. Read `README.md`.
2. Read `STATUS.md`.
3. Read `docs/research-notes.md`.
4. Run `npm run verify` after meaningful changes.

Engineering rules:

- Import UI framework APIs from `Asterism`.
- Keep app state explicit and pure; put browser persistence behind small FFI modules.
- Do not add remote telemetry, model calls, accounts, or secret-bearing configuration without updating the public claim boundary.
- Do not widen cognitive or health claims without adding evidence and revising `STATUS.md`.
- Keep the first screen as the usable training surface, not a marketing page.
