# Research Notes

This note records the evidence boundary used for the MVP design. It is not a medical review.

## Working Memory Training

- A 2026 npj Digital Medicine meta-analysis of computerized working-memory training reported moderate behavioral gains versus controls and neuroimaging changes compatible with reduced recruitment after training. Source: Li, Liu, and Chen, 2026, https://www.nature.com/articles/s41746-026-02478-9
- A 2024 second-order meta-analysis focused on healthy adults found a small but significant average working-memory improvement across included intervention meta-analyses. Source: https://www.mdpi.com/2079-3200/12/11/114
- Evidence is much weaker for broad transfer. A randomized comparison in healthy young adults found support for null effects on untrained cognitive abilities. Source: https://pubmed.ncbi.nlm.nih.gov/28558000/
- For ADHD, blinded/objective outcomes in a 2023 meta-analysis were conservative; this app therefore avoids treatment claims. Source: https://pubmed.ncbi.nlm.nih.gov/36977764/

Design consequence: train near-task skills and keep levels per drill. Do not claim IQ gains, ADHD treatment, or broad transfer.

## Reading Speed And Comprehension

- Rayner et al. argue that reading speed is constrained by word identification and comprehension, not merely eye movement mechanics; regressions can repair comprehension. Source: https://journals.sagepub.com/doi/10.1177/1529100615623267
- A PLOS One study on Japanese novels found faster trained readers in some cases, but speed and comprehension tradeoffs remained visible across participants. Source: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0036091

Design consequence: the app does not optimize raw WPM. Dense-reading rounds hide text before questioning so the score measures retained meaning, not visual search.

## Learning Practice

- Dunlosky et al. rated practice testing and distributed practice as high-utility techniques across broad learning settings. Source: https://gwern.net/doc/psychology/spaced-repetition/2013-dunlosky.pdf

Design consequence: rounds are short, scored, and repeatable. Future versions should add spaced baseline retests instead of only making sessions longer.

## Gamification

- A 2023 meta-analysis framed gamified learning through autonomy, competence, and relatedness, and emphasized challenge just outside the comfort zone. Source: https://link.springer.com/article/10.1007/s11423-023-10337-7
- A cognitive-assessment/training gamification framework recommends objective engagement measures as well as subjective flow/self-determination measures. Source: https://pmc.ncbi.nlm.nih.gov/articles/PMC8170558/

Design consequence: the MVP uses private streaks, immediate feedback, adaptive levels, and mode choice. It avoids global leaderboards because speed-without-comprehension is the wrong target.
