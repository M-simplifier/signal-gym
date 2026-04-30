# Publication Audit

Date: 2026-04-30

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
- GitHub Pages must be verified after the first deploy workflow completes.
