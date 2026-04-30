# Design

## Core Reframing

The product is not a generic brain-training arcade. It is a daily practice board for people who must read, hold, and audit dense AI-generated text under time pressure.

## Default Attractors Rejected

- Generic brain game: easy to ship, weak transfer story.
- Dual n-back clone: research-adjacent, too narrow and dull for the target user.
- Speed-reading app: optimizes a misleading number if comprehension is not central.
- Duolingo-style streak loop: visible habit pressure without enough intrinsic repetition quality.
- Medical ADHD app: outside the evidence and regulatory boundary.

## Primary Bet

Use three short drills that stay close to the real target activity:

- `Claim Gate`: contradiction and unsupported-claim detection.
- `Trace Stack`: token-order retention under time pressure.
- `Dense Read`: brief dense passage, hidden text, then detail/gist recall.

The loop should feel closer to guitar/Rubik repetition than school worksheets: immediate feedback, compact rounds, visible mastery, and no moralizing copy.

## MVP Rules

- One session must be useful in 4 to 7 minutes.
- Difficulty adapts per drill, not globally.
- Accuracy matters more than raw speed.
- Local persistence is enough for v0.
- Public text must distinguish near-task practice from far-transfer claims.

## Drill Separation

The drill names must not hide the task contract.

- `Claim Gate`: an audit loop: evidence, risky claim, stop decision.
- `Trace Stack`: a sequence loop: ordered tokens, mask, target recall.
- `Dense Read`: a reading loop: dense passage, hidden text, memory answer.

If two drills feel like the same multiple-choice surface, the UI has failed even if the answer data differs.

## Rejected But Valuable

- User-imported AI documents: high transfer value, but needs privacy and prompt-safety design.
- Baseline tests: important for evidence, but they add friction before the daily loop is proven.
- Social competition: may improve retention for some users, but risks optimizing speed and streak anxiety.
- Real model-generated rounds: promising later, but not needed for a local-first public-safe MVP.

## Invalidation Conditions

- The user does not want to replay a session after the first day.
- Scores improve but the exercises feel unrelated to AI document work.
- The easiest way to improve is memorizing item templates rather than improving attention/retention.
- Timers create avoidance instead of flow.
