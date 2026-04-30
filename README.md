# Signal Gym

Signal Gym is a local-first PureScript SPA for short, high-density practice aimed at AI-era reading and review work.

The prototype trains three near-task skills:

- `Claim Gate`: spot claim drift, contradictions, and unsupported review summaries.
- `Trace Stack`: hold token sequences under mild time pressure.
- `Dense Read`: read a compact passage, hide it, and answer from memory.

It does not claim to diagnose or treat ADHD, raise IQ, or guarantee far-transfer cognitive gains.

## Run Locally

```sh
npm install
npm run verify
npm run preview
```

Then open `http://127.0.0.1:4174/`.

## Stack

- PureScript 0.15
- Asterism via local workspace path: `../asterism`
- Static SPA output in `dist/`
- GitHub Pages deployment through Actions

## Evidence Boundary

The design follows a conservative reading of the literature: trained-task gains and near-task practice are plausible; broad cognitive-transfer claims are not made here. See [docs/research-notes.md](docs/research-notes.md) and [docs/design.md](docs/design.md).

## Privacy

Progress is stored in browser `localStorage`. There is no account system, telemetry, remote model call, or server.
