# Publication Audit

Date: 2026-04-30

Repository: https://github.com/M-simplifier/signal-gym
Pages: https://m-simplifier.github.io/signal-gym/

## Public Boundary

Included:

- Source code for the PureScript SPA
- Static build and GitHub Pages workflow
- Research/design notes with conservative claim boundaries
- MIT license and issue-first contribution posture

Excluded:

- Local build outputs
- `node_modules`
- `.spago` and PureScript `output`
- Screenshots and local Playwright artifacts
- Environment files and secrets

## Claim Audit

Allowed claims:

- Evidence-informed prototype
- Local-first training app
- Near-task practice for attention, working memory, dense reading, and claim checking

Blocked claims:

- ADHD treatment
- Medical diagnosis
- IQ improvement
- Guaranteed working-memory improvement
- Guaranteed productivity improvement
- Proven far transfer

## Secret And Local-Path Scan

Patterns checked by `npm run smoke`:

- OpenAI-style secret strings
- GitHub token prefixes
- private-key headers
- private config path leakage

Known local path:

- `spago.yaml` references `../asterism` as an intentional repo-first pre-beta Asterism dependency. This is documented in `README.md`.

## Residual Risks

- The exercise corpus is hand-authored and small; users may memorize it after repeated play.
- No longitudinal user data exists yet.
- The app has not been externally validated.
- `npm audit --omit=dev` reports 0 production vulnerabilities. Full dev audit still reports transitive installer issues under the PureScript npm package; they do not ship in the static SPA artifact.
- The first GitHub Pages deploy was verified with HTTP 200 and a live Playwright screenshot pass.
- GitHub Actions currently emits a Node.js 20 action-runtime deprecation warning for upstream actions. The workflows pass; revisit action major versions when their Node 24-compatible releases are available.
