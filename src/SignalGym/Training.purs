module SignalGym.Training
  ( Drill(..)
  , Mode(..)
  , Stage(..)
  , Option
  , Round
  , Feedback
  , DrillScore
  , Profile
  , Session
  , emptyProfile
  , startSession
  , currentRound
  , revealCurrent
  , tickSession
  , answerCurrent
  , advanceAfterFeedback
  , completeProfile
  , sessionAccuracy
  , roundCount
  , modeLabel
  , drillLabel
  , drillClass
  , levelForDrill
  , levelLabel
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String

data Drill
  = Gate
  | Trace
  | Read

derive instance eqDrill :: Eq Drill

data Mode
  = DailyMix
  | GateOnly
  | TraceOnly
  | ReadOnly

derive instance eqMode :: Eq Mode

data Stage
  = Encoding
  | Answering
  | Feedback
  | Complete

derive instance eqStage :: Eq Stage

type Option =
  { label :: String
  , detail :: String
  }

type Round =
  { id :: String
  , drill :: Drill
  , title :: String
  , load :: Int
  , stimulus :: String
  , prompt :: String
  , options :: Array Option
  , correctIndex :: Int
  , encodeSeconds :: Int
  , answerSeconds :: Int
  , rationale :: String
  }

type Feedback =
  { correct :: Boolean
  , selected :: Maybe Int
  , gain :: Int
  , label :: String
  , rationale :: String
  }

type DrillScore =
  { correct :: Int
  , total :: Int
  }

type Profile =
  { xp :: Int
  , streak :: Int
  , lastDay :: String
  , todayKey :: String
  , yesterdayKey :: String
  , sessions :: Int
  , bestScore :: Int
  , focusMinutes :: Int
  , gateLevel :: Int
  , traceLevel :: Int
  , readLevel :: Int
  }

type Session =
  { mode :: Mode
  , rounds :: Array Round
  , index :: Int
  , stage :: Stage
  , remaining :: Int
  , score :: Int
  , combo :: Int
  , correct :: Int
  , answered :: Int
  , gate :: DrillScore
  , trace :: DrillScore
  , read :: DrillScore
  , feedback :: Maybe Feedback
  }

emptyScore :: DrillScore
emptyScore = { correct: 0, total: 0 }

emptyProfile :: Profile
emptyProfile =
  { xp: 0
  , streak: 0
  , lastDay: ""
  , todayKey: ""
  , yesterdayKey: ""
  , sessions: 0
  , bestScore: 0
  , focusMinutes: 0
  , gateLevel: 2
  , traceLevel: 2
  , readLevel: 2
  }

totalRounds :: Mode -> Int
totalRounds mode = case mode of
  DailyMix -> 12
  GateOnly -> 10
  TraceOnly -> 10
  ReadOnly -> 10

startSession :: Profile -> Mode -> Session
startSession profile mode =
  let
    rounds =
      generateRounds profile mode

    firstRemaining =
      case Array.index rounds 0 of
        Just round -> round.encodeSeconds
        Nothing -> 0
  in
    { mode
    , rounds
    , index: 0
    , stage: Encoding
    , remaining: firstRemaining
    , score: 0
    , combo: 0
    , correct: 0
    , answered: 0
    , gate: emptyScore
    , trace: emptyScore
    , read: emptyScore
    , feedback: Nothing
    }

currentRound :: Session -> Maybe Round
currentRound session =
  Array.index session.rounds session.index

roundCount :: Session -> Int
roundCount session =
  Array.length session.rounds

revealCurrent :: Session -> Session
revealCurrent session =
  case currentRound session of
    Nothing ->
      session { stage = Complete, remaining = 0 }

    Just round ->
      if session.stage == Encoding then
        session { stage = Answering, remaining = round.answerSeconds, feedback = Nothing }
      else
        session

tickSession :: Session -> Session
tickSession session =
  case session.stage of
    Encoding ->
      if session.remaining <= 1 then
        revealCurrent session
      else
        session { remaining = session.remaining - 1 }

    Answering ->
      if session.remaining <= 1 then
        answerCurrent Nothing session
      else
        session { remaining = session.remaining - 1 }

    _ ->
      session

answerCurrent :: Maybe Int -> Session -> Session
answerCurrent selected session =
  if session.stage /= Answering then
    session
  else
    case currentRound session of
      Nothing ->
        session { stage = Complete, remaining = 0 }

      Just round ->
        let
          wasCorrect =
            selected == Just round.correctIndex

          nextCombo =
            if wasCorrect then session.combo + 1 else 0

          gain =
            if wasCorrect then
              80 + round.load * 12 + session.remaining * 4 + session.combo * 10
            else
              0

          nextScore =
            session.score + gain

          nextStats =
            addDrillResult round.drill wasCorrect session

          feedback =
            { correct: wasCorrect
            , selected
            , gain
            , label: feedbackLabel round.drill wasCorrect
            , rationale: round.rationale
            }
        in
          nextStats
            { stage = Feedback
            , remaining = 0
            , score = nextScore
            , combo = nextCombo
            , correct = session.correct + (if wasCorrect then 1 else 0)
            , answered = session.answered + 1
            , feedback = Just feedback
            }

advanceAfterFeedback :: Session -> Session
advanceAfterFeedback session =
  if session.stage /= Feedback then
    session
  else
    let
      nextIndex = session.index + 1
    in
      case Array.index session.rounds nextIndex of
        Nothing ->
          session { stage = Complete, index = nextIndex, remaining = 0, feedback = Nothing }

        Just nextRound ->
          session
            { stage = Encoding
            , index = nextIndex
            , remaining = nextRound.encodeSeconds
            , feedback = Nothing
            }

completeProfile :: Profile -> Session -> Profile
completeProfile profile session =
  let
    nextStreak =
      if profile.lastDay == profile.todayKey then
        profile.streak
      else if profile.lastDay == profile.yesterdayKey then
        profile.streak + 1
      else
        1

    nextBest =
      max profile.bestScore session.score
  in
    profile
      { xp = profile.xp + session.score
      , streak = nextStreak
      , lastDay = profile.todayKey
      , sessions = profile.sessions + 1
      , bestScore = nextBest
      , focusMinutes = profile.focusMinutes + estimatedMinutes session
      , gateLevel = adaptLevel profile.gateLevel session.gate
      , traceLevel = adaptLevel profile.traceLevel session.trace
      , readLevel = adaptLevel profile.readLevel session.read
      }

sessionAccuracy :: Session -> Int
sessionAccuracy session =
  if session.answered == 0 then 0 else div (session.correct * 100) session.answered

estimatedMinutes :: Session -> Int
estimatedMinutes session =
  max 1 (div (roundCount session * 28) 60)

adaptLevel :: Int -> DrillScore -> Int
adaptLevel current stats =
  if stats.total == 0 then
    current
  else if stats.correct * 100 >= stats.total * 85 then
    clampInt 1 9 (current + 1)
  else if stats.correct * 100 <= stats.total * 55 then
    clampInt 1 9 (current - 1)
  else
    current

addDrillResult :: Drill -> Boolean -> Session -> Session
addDrillResult drill wasCorrect session =
  let
    add score =
      { correct: score.correct + (if wasCorrect then 1 else 0)
      , total: score.total + 1
      }
  in
    case drill of
      Gate -> session { gate = add session.gate }
      Trace -> session { trace = add session.trace }
      Read -> session { read = add session.read }

feedbackLabel :: Drill -> Boolean -> String
feedbackLabel drill correct = case drill, correct of
  Gate, true -> "Stopped"
  Gate, false -> "Leaked"
  Trace, true -> "Held"
  Trace, false -> "Dropped"
  Read, true -> "Recalled"
  Read, false -> "Lost"

generateRounds :: Profile -> Mode -> Array Round
generateRounds profile mode =
  map (\i -> roundFor profile mode i) (Array.range 0 (totalRounds mode - 1))

roundFor :: Profile -> Mode -> Int -> Round
roundFor profile mode index =
  let
    drill =
      drillFor mode index

    level =
      levelForDrill profile drill
  in
    case drill of
      Gate -> gateRound level index
      Trace -> traceRound level index
      Read -> readRound level index

drillFor :: Mode -> Int -> Drill
drillFor mode index = case mode of
  DailyMix ->
    case mod index 3 of
      0 -> Gate
      1 -> Trace
      _ -> Read

  GateOnly -> Gate
  TraceOnly -> Trace
  ReadOnly -> Read

levelForDrill :: Profile -> Drill -> Int
levelForDrill profile drill = case drill of
  Gate -> profile.gateLevel
  Trace -> profile.traceLevel
  Read -> profile.readLevel

modeLabel :: Mode -> String
modeLabel mode = case mode of
  DailyMix -> "Daily Mix"
  GateOnly -> "Claim Gate"
  TraceOnly -> "Trace Stack"
  ReadOnly -> "Dense Read"

drillLabel :: Drill -> String
drillLabel drill = case drill of
  Gate -> "Claim Gate"
  Trace -> "Trace Stack"
  Read -> "Dense Read"

drillClass :: Drill -> String
drillClass drill = case drill of
  Gate -> "gate"
  Trace -> "trace"
  Read -> "read"

levelLabel :: Int -> String
levelLabel level =
  "Lv." <> show level

type GateCase =
  { source :: String
  , prompt :: String
  , options :: Array Option
  , correctIndex :: Int
  , rationale :: String
  }

gateRound :: Int -> Int -> Round
gateRound level index =
  let
    item =
      pick defaultGate gateCases (index + level)
  in
    { id: "gate-" <> show level <> "-" <> show index
    , drill: Gate
    , title: "Claim Gate"
    , load: level
    , stimulus: item.source
    , prompt: item.prompt
    , options: item.options
    , correctIndex: item.correctIndex
    , encodeSeconds: clampInt 9 18 (18 - div level 2)
    , answerSeconds: clampInt 8 15 (15 - div level 3)
    , rationale: item.rationale
    }

defaultGate :: GateCase
defaultGate =
  { source: "Build log: parser latency fell from 1.8s to 1.1s. Memory rose from 420MB to 470MB. The benchmark used 200 documents."
  , prompt: "Which review comment should stop first?"
  , options:
      [ { label: "Latency gain is unsupported", detail: "The numbers support it" }
      , { label: "Memory improved", detail: "The log says memory increased" }
      , { label: "Document count is missing", detail: "The count is present" }
      , { label: "Whole result is invalid", detail: "Part of it is usable" }
      ]
  , correctIndex: 1
  , rationale: "Latency improved, but memory increased. Calling memory an improvement reverses the evidence."
  }

gateCases :: Array GateCase
gateCases =
  [ defaultGate
  , { source: "Experiment note: prompt A solved 31/40 cases. Prompt B solved 34/40 cases but produced 6 unsupported citations. The report recommends B for citation-heavy tasks."
    , prompt: "Where is the risky drift?"
    , options:
        [ { label: "B solved more cases", detail: "That is true" }
        , { label: "B for citation-heavy tasks", detail: "Conflicts with unsupported citations" }
        , { label: "A accuracy", detail: "31/40 can be computed" }
        , { label: "Experiment size", detail: "Small, but not the main contradiction" }
        ]
    , correctIndex: 1
    , rationale: "A citation-heavy task should not prefer the variant with more unsupported citations."
    }
  , { source: "Release draft: mobile layout fixed overflow in the editor and the dashboard. The settings modal was not retested. The summary says all responsive regressions are closed."
    , prompt: "What should be corrected in the summary?"
    , options:
        [ { label: "Keep all regressions closed", detail: "A modal was not retested" }
        , { label: "Settings modal unverified", detail: "Names the remaining gap" }
        , { label: "Remove mobile wording", detail: "The issue is mobile" }
        , { label: "Mark dashboard unresolved", detail: "The draft says it was fixed" }
        ]
    , correctIndex: 1
    , rationale: "The modal was not retested, so all closed is too strong."
    }
  , { source: "Research digest: Study X improved trained n-back scores. The authors found no statistically reliable gain on fluid reasoning. The digest headline says broad intelligence improved."
    , prompt: "What is the strongest red flag?"
    , options:
        [ { label: "Trained-task gain", detail: "This matches the text" }
        , { label: "Fluid reasoning transfer", detail: "Contradicts no reliable gain" }
        , { label: "The n-back name", detail: "Not a problem by itself" }
        , { label: "Statistical wording", detail: "The text includes it" }
        ]
    , correctIndex: 1
    , rationale: "The headline turns trained-task improvement into broad intelligence improvement."
    }
  , { source: "Incident note: API errors began at 09:42. Deploy finished at 09:55. Database failover started at 09:39 and recovered at 10:08. Draft root cause says deploy caused the outage."
    , prompt: "Which claim has weak support?"
    , options:
        [ { label: "09:42 start", detail: "Recorded directly" }
        , { label: "Deploy caused outage", detail: "Deploy finished after errors began" }
        , { label: "DB failover", detail: "It preceded the errors" }
        , { label: "10:08 recovery", detail: "Recorded directly" }
        ]
    , correctIndex: 1
    , rationale: "Errors began before the deploy finished, so a simple deploy cause is weak."
    }
  , { source: "Design note: remove the onboarding screen, keep the daily button visible, and reduce first action from five choices to two. The critique says the change increases first-use cognitive load."
    , prompt: "What is weak in the critique?"
    , options:
        [ { label: "Onboarding removal", detail: "Could raise load" }
        , { label: "Fewer choices", detail: "Usually lowers first-action load" }
        , { label: "Visible daily button", detail: "A visibility choice" }
        , { label: "First action", detail: "The target of review" }
        ]
    , correctIndex: 1
    , rationale: "Reducing five choices to two is not strong evidence for increased load."
    }
  , { source: "Eval table: Model R gives concise answers with 82% factual pass rate. Model S gives longer answers with 91% pass rate. For a terse status bot, the recommendation is S without mentioning answer length."
    , prompt: "What should the review require?"
    , options:
        [ { label: "Remove S pass rate", detail: "It is important evidence" }
        , { label: "Answer-length tradeoff", detail: "May conflict with a terse bot" }
        , { label: "Always choose R", detail: "Not justified" }
        , { label: "Reject both", detail: "Too strong" }
        ]
    , correctIndex: 1
    , rationale: "For a terse bot, answer length is part of the evaluation, not noise."
    }
  , { source: "Security checklist: no API keys in source. The .env file is ignored. A sample config contains KEY-LIKE-VALUE as a placeholder. The audit says no credential-shaped strings are present."
    , prompt: "What should stop the audit?"
    , options:
        [ { label: ".env is ignored", detail: "Good" }
        , { label: "Secret-like placeholder", detail: "Unsafe on a public surface" }
        , { label: "No keys in source", detail: "Claimed in the checklist" }
        , { label: "Sample config exists", detail: "Existence is not the issue" }
        ]
    , correctIndex: 1
    , rationale: "Even fake secret-looking strings should be caught before publication."
    }
  ]

type TraceToken =
  { mark :: String
  , value :: Int
  }

traceRound :: Int -> Int -> Round
traceRound level index =
  let
    seed =
      level * 7 + index * 5

    size =
      clampInt 4 8 (3 + level)

    tokens =
      map (\i -> tokenAt (seed + i * 3)) (Array.range 0 (size - 1))

    nBack =
      clampInt 2 4 (2 + mod (level + index) 3)

    targetIndex =
      max 0 (size - nBack)

    target =
      fromMaybe (tokenAt seed) (Array.index tokens targetIndex)

    correct =
      tokenLabel target

    distractors =
      [ tokenLabel (tokenAt (seed + 11))
      , tokenLabel (tokenAt (seed + 17))
      , tokenLabel (tokenAt (seed + 23))
      ]

    correctIndex =
      mod (seed + level) 4
  in
    { id: "trace-" <> show level <> "-" <> show index
    , drill: Trace
    , title: "Trace Stack"
    , load: level
    , stimulus: String.joinWith "   " (map tokenLabel tokens)
    , prompt: "Which token was " <> show nBack <> " from the end?"
    , options: placeCorrect correctIndex { label: correct, detail: "Choose from the held sequence" } distractors
    , correctIndex
    , encodeSeconds: clampInt 5 12 (12 - div level 2)
    , answerSeconds: clampInt 6 12 (12 - div level 3)
    , rationale: "Count backward from the final token as 1. The target token is " <> correct <> "."
    }

tokenAt :: Int -> TraceToken
tokenAt seed =
  { mark: pick "K" marks seed
  , value: fromMaybe 1 (Array.index values (mod seed (Array.length values)))
  }

tokenLabel :: TraceToken -> String
tokenLabel token =
  token.mark <> show token.value

marks :: Array String
marks = [ "K", "R", "M", "S", "T", "L", "P", "N", "C", "V", "H", "Q" ]

values :: Array Int
values = [ 2, 7, 4, 9, 1, 6, 3, 8, 5, 0, 11, 13 ]

type ReadCase =
  { passage :: String
  , prompt :: String
  , options :: Array Option
  , correctIndex :: Int
  , rationale :: String
  }

readRound :: Int -> Int -> Round
readRound level index =
  let
    item =
      pick defaultRead readCases (index + level * 2)
  in
    { id: "read-" <> show level <> "-" <> show index
    , drill: Read
    , title: "Dense Read"
    , load: level
    , stimulus: item.passage
    , prompt: item.prompt
    , options: item.options
    , correctIndex: item.correctIndex
    , encodeSeconds: clampInt 12 24 (24 - level)
    , answerSeconds: clampInt 8 16 (16 - div level 2)
    , rationale: item.rationale
    }

defaultRead :: ReadCase
defaultRead =
  { passage: "The review pipeline accepts a model answer only after two checks: factual anchors must be cited, and risk labels must match the evidence tier. In the latest run, citations passed, but three high-risk labels were attached to low-evidence notes."
  , prompt: "Which condition failed in the latest run?"
  , options:
      [ { label: "citations", detail: "They passed" }
      , { label: "risk labels", detail: "They mismatched the evidence tier" }
      , { label: "model answer count", detail: "No count is given" }
      , { label: "pipeline entry", detail: "Not a failed condition" }
      ]
  , correctIndex: 1
  , rationale: "Citations passed. Risk labels failed to match the evidence tier."
  }

readCases :: Array ReadCase
readCases =
  [ defaultRead
  , { passage: "A daily training block is marked complete when the user finishes either one mixed session or two focused sessions. Calibration is updated only from sessions with at least eight answered rounds, so abandoned attempts do not lower the next target."
    , prompt: "When is calibration updated?"
    , options:
        [ { label: "Only mixed sessions", detail: "Focused sessions can count too" }
        , { label: "At least 8 answered rounds", detail: "Stated directly" }
        , { label: "Abandoned attempts", detail: "They do not update it" }
        , { label: "Two mixed sessions", detail: "Not stated" }
        ]
    , correctIndex: 1
    , rationale: "Completion and calibration are separate. Calibration needs at least 8 answered rounds."
    }
  , { passage: "The app avoids global leaderboards because speed without comprehension can become the wrong target. It keeps streaks private, shows per-drill levels, and rewards perfect short runs more than long fatigued runs."
    , prompt: "What is not included?"
    , options:
        [ { label: "private streaks", detail: "Included" }
        , { label: "per-drill levels", detail: "Included" }
        , { label: "global leaderboards", detail: "Avoided" }
        , { label: "short-run reward", detail: "Included" }
        ]
    , correctIndex: 2
    , rationale: "Leaderboards are avoided because they can reward speed without comprehension."
    }
  , { passage: "For public claims, the project distinguishes trained-task gains, near transfer, and far transfer. The first two may be reported with internal data. Far transfer remains a research question unless measured outside the app."
    , prompt: "What is not claimed without outside measurement?"
    , options:
        [ { label: "trained-task gains", detail: "Internal data may support this" }
        , { label: "near transfer", detail: "Internal data may support this" }
        , { label: "far transfer", detail: "Still a research question" }
        , { label: "task completion", detail: "Can be measured in app" }
        ]
    , correctIndex: 2
    , rationale: "Far transfer is not claimed without measurement outside the app."
    }
  , { passage: "The reading drill presents dense text briefly, then hides it before questioning. This prevents simple visual search and shifts the work toward gist retention, detail binding, and confidence under time pressure."
    , prompt: "Why does the text get hidden?"
    , options:
        [ { label: "Prevent visual search", detail: "Stated directly" }
        , { label: "Decorative effect", detail: "Not the purpose" }
        , { label: "Reduce word count", detail: "Not stated" }
        , { label: "Reduce network calls", detail: "Unrelated" }
        ]
    , correctIndex: 0
    , rationale: "If the text stays visible, the task becomes search instead of retained comprehension."
    }
  , { passage: "Adaptive difficulty should move slowly. A single excellent run raises one level, but a weak run only lowers the relevant drill. This keeps challenge near the edge without punishing one bad modality across the whole app."
    , prompt: "How is a weak run handled?"
    , options:
        [ { label: "Lower every drill", detail: "Not stated" }
        , { label: "Lower only the relevant drill", detail: "Stated directly" }
        , { label: "Always keep level", detail: "It can drop" }
        , { label: "Raise two levels", detail: "Opposite direction" }
        ]
    , correctIndex: 1
    , rationale: "A weak run affects the relevant drill without punishing the whole app."
    }
  , { passage: "A useful AI-era reader does not merely read faster. They preserve unresolved assumptions, notice claim drift, and decide when a paragraph deserves slow rereading. Raw words-per-minute is therefore logged only with accuracy."
    , prompt: "Why is WPM not a standalone target?"
    , options:
        [ { label: "Slow rereading can matter", detail: "Part of the target skill" }
        , { label: "It must pair with accuracy", detail: "Standalone speed is risky" }
        , { label: "Claim drift is unnecessary", detail: "It is necessary" }
        , { label: "Assumptions should be discarded", detail: "Opposite of the passage" }
        ]
    , correctIndex: 1
    , rationale: "Speed can damage comprehension, so it is meaningful only with accuracy."
    }
  , { passage: "The prototype stores progress locally. No account, telemetry, remote model call, or health diagnosis is included. Export and baseline tests are planned after the local training loop proves daily use."
    , prompt: "What is not included now?"
    , options:
        [ { label: "local progress", detail: "Included" }
        , { label: "remote model call", detail: "Not included" }
        , { label: "daily loop", detail: "Core of the prototype" }
        , { label: "planned export", detail: "Future work" }
        ]
    , correctIndex: 1
    , rationale: "This MVP is local-first and makes no remote model calls."
    }
  ]

placeCorrect :: Int -> Option -> Array String -> Array Option
placeCorrect correctIndex correct distractors =
  map optionAt (Array.range 0 3)
  where
  optionAt i =
    if i == correctIndex then
      correct
    else
      let
        offset =
          if i < correctIndex then i else i - 1

        label =
          fromMaybe correct.label (Array.index distractors offset)
      in
        { label, detail: "near distractor" }

pick :: forall a. a -> Array a -> Int -> a
pick fallback items rawIndex =
  let
    count =
      Array.length items

    index =
      if rawIndex < 0 then 0 - rawIndex else rawIndex
  in
    if count == 0 then
      fallback
    else
      fromMaybe fallback (Array.index items (mod index count))

clampInt :: Int -> Int -> Int -> Int
clampInt low high value =
  max low (min high value)
