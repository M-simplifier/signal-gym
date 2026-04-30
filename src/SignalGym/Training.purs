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
  , startSessionWithSeed
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
  , domain :: String
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
  startSessionWithSeed profile mode (profileSeed profile mode)

startSessionWithSeed :: Profile -> Mode -> Int -> Session
startSessionWithSeed profile mode seed =
  let
    sessionSeed =
      seedForSession profile mode seed

    rounds =
      generateRounds profile mode sessionSeed

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

generateRounds :: Profile -> Mode -> Int -> Array Round
generateRounds profile mode sessionSeed =
  map (\i -> roundFor profile mode sessionSeed i) (Array.range 0 (totalRounds mode - 1))

roundFor :: Profile -> Mode -> Int -> Int -> Round
roundFor profile mode sessionSeed index =
  let
    drill =
      drillFor mode sessionSeed index

    level =
      levelForDrill profile drill

    roundSeed =
      mixSeed sessionSeed (index * 677 + level * 41 + drillCode drill * 101)
  in
    case drill of
      Gate -> gateRound level roundSeed index
      Trace -> traceRound level roundSeed index
      Read -> readRound level roundSeed index

drillFor :: Mode -> Int -> Int -> Drill
drillFor mode seed index = case mode of
  DailyMix ->
    case positiveMod (seed + index * 7) 3 of
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

profileSeed :: Profile -> Mode -> Int
profileSeed profile mode =
  let
    sessionsPart =
      positiveMod profile.sessions 100003

    xpPart =
      positiveMod profile.xp 100003

    bestPart =
      positiveMod profile.bestScore 100003
  in
    mixSeed
      (sessionsPart * 997 + xpPart * 31 + bestPart * 17)
      (modeCode mode * 257 + profile.gateLevel * 11 + profile.traceLevel * 13 + profile.readLevel * 17)

seedForSession :: Profile -> Mode -> Int -> Int
seedForSession profile mode seed =
  let
    sessionsPart =
      positiveMod profile.sessions 100003

    focusPart =
      positiveMod profile.focusMinutes 100003
  in
    mixSeed
      seed
      (profileSeed profile mode + sessionsPart * 409 + focusPart * 37)

modeCode :: Mode -> Int
modeCode mode = case mode of
  DailyMix -> 11
  GateOnly -> 23
  TraceOnly -> 37
  ReadOnly -> 53

drillCode :: Drill -> Int
drillCode drill = case drill of
  Gate -> 3
  Trace -> 5
  Read -> 7

mixSeed :: Int -> Int -> Int
mixSeed seed salt =
  let
    base =
      positiveMod seed 1000003

    saltPart =
      positiveMod salt 100003
  in
    positiveMod (base * 73 + saltPart * 997 + 104729) 1000003

positiveMod :: Int -> Int -> Int
positiveMod value count =
  if count <= 0 then
    0
  else
    let
      remainder =
        mod value count
    in
      if remainder < 0 then remainder + count else remainder

caseIndex :: Int -> Int -> Int -> Int -> Int
caseIndex seed index level count =
  positiveMod (seed + index * 7 + level * 11) count

rotateRoundOptions :: Int -> Int -> Array Option -> { options :: Array Option, correctIndex :: Int }
rotateRoundOptions seed correctIndex options =
  let
    count =
      Array.length options

    offset =
      positiveMod seed count
  in
    if count == 0 then
      { options, correctIndex }
    else
      { options: Array.drop offset options <> Array.take offset options
      , correctIndex: positiveMod (correctIndex - offset) count
      }

type GateCase =
  { domain :: String
  , source :: String
  , prompt :: String
  , options :: Array Option
  , correctIndex :: Int
  , rationale :: String
  }

gateRound :: Int -> Int -> Int -> Round
gateRound level seed index =
  let
    pickedIndex =
      caseIndex seed index level (Array.length gateCases)

    item =
      pick defaultGate gateCases pickedIndex

    rotated =
      rotateRoundOptions (mixSeed seed pickedIndex) item.correctIndex item.options
  in
    { id: "gate-" <> show level <> "-" <> show pickedIndex <> "-" <> show index
    , drill: Gate
    , title: "Claim Gate"
    , load: level
    , domain: item.domain
    , stimulus: item.source
    , prompt: item.prompt
    , options: rotated.options
    , correctIndex: rotated.correctIndex
    , encodeSeconds: clampInt 9 18 (18 - div level 2)
    , answerSeconds: clampInt 8 15 (15 - div level 3)
    , rationale: item.rationale
    }

defaultGate :: GateCase
defaultGate =
  {
  domain: "software"
    , source: "Build note: enabling the index cache reduced median search time from 420 ms to 260 ms. Peak memory rose from 610 MB to 760 MB. The test used the same 8,000 synthetic records. The release summary says the cache improves speed and reduces memory use."
    , prompt: "Which claim should stop the summary?"
    , options:
        [ { label: "Speed improved", detail: "Supported by lower median time" }
        , { label: "Memory reduced", detail: "Contradicts the higher peak memory" }
        , { label: "Same records used", detail: "Stated in the note" }
        , { label: "Cache was enabled", detail: "Stated in the note" } ]
    , correctIndex: 1
    , rationale: "The source supports faster search, but peak memory increased rather than decreased."
    }

gateCases :: Array GateCase
gateCases =
  [ defaultGate
  , {
  domain: "research-methods"
    , source: "Pilot report: 48 volunteers completed a lab sorting task. Group A saw accuracy rise by 6 points after guided examples. The report did not test workplace performance or long-term retention. The abstract says the method is proven to improve operational quality in real teams."
    , prompt: "Where is the overstatement?"
    , options:
        [ { label: "48 volunteers", detail: "Sample size is stated" }
        , { label: "Lab sorting task", detail: "This is the actual setting" }
        , { label: "Real-team operational quality", detail: "Not tested by the pilot" }
        , { label: "Accuracy rose 6 points", detail: "Supported by the report" } ]
    , correctIndex: 2
    , rationale: "The pilot measured a lab task, not workplace performance in real teams."
    }
  , {
  domain: "operations"
    , source: "Shift log: queue B started with 22 requests. Staff resolved 18, escalated 3 for manager review, and parked 1 because the customer record was locked. The handoff note says queue B is fully cleared and needs no follow-up."
    , prompt: "Which handoff claim is risky?"
    , options:
        [ { label: "18 were resolved", detail: "Matches the log" }
        , { label: "3 need manager review", detail: "Matches the log" }
        , { label: "Queue fully cleared", detail: "Escalated and parked items remain" }
        , { label: "Record lock blocked one", detail: "Matches the log" } ]
    , correctIndex: 2
    , rationale: "Four requests still need action, so fully cleared is false."
    }
  , {
  domain: "logistics"
    , source: "Dock record: route L expected 50 crates. The dock received 47 crates before cutoff; 2 were held at the south yard, and 1 label was unreadable. Temperature logs stayed within range for all scanned crates. The shipment report says the route arrived complete and fully identified."
    , prompt: "What should be corrected?"
    , options:
        [ { label: "Temperature within range", detail: "Supported for scanned crates" }
        , { label: "Complete arrival", detail: "Only 47 of 50 arrived" }
        , { label: "South yard hold", detail: "Stated directly" }
        , { label: "Cutoff timing", detail: "Not contradicted" } ]
    , correctIndex: 1
    , rationale: "The shipment was not complete at cutoff, and one crate was not fully identified."
    }
  , {
  domain: "finance-budgeting"
    , source: "Budget sheet: the training line has 12,000 credits approved. Booked workshops cost 9,500. A pending venue invoice is estimated at 3,100 but not yet approved. The update says the training budget has 2,500 credits free after all known costs."
    , prompt: "Which budget claim is unsupported?"
    , options:
        [ { label: "12,000 approved", detail: "Stated in the sheet" }
        , { label: "9,500 booked", detail: "Stated in the sheet" }
        , { label: "2,500 free after all costs", detail: "Ignores the pending 3,100 estimate" }
        , { label: "Venue invoice pending", detail: "Stated in the sheet" } ]
    , correctIndex: 2
    , rationale: "After including the known pending estimate, the line would exceed the approved amount."
    }
  , {
  domain: "security"
    , source: "Security review: production tokens are absent from the repository. The example file includes TOKEN-SHAPED-PLACEHOLDER as a fake placeholder for setup tests. Screenshots show no secrets. The public release note says the project contains no token-shaped strings anywhere."
    , prompt: "What is the release-note problem?"
    , options:
        [ { label: "Production tokens absent", detail: "Supported by the review" }
        , { label: "Screenshots are clean", detail: "Supported by the review" }
        , { label: "No token-shaped strings", detail: "Contradicted by the placeholder" }
        , { label: "Example file exists", detail: "Not a problem by itself" } ]
    , correctIndex: 2
    , rationale: "A fake placeholder can still be token-shaped, so the absolute claim is false."
    }
  , {
  domain: "accessibility"
    , source: "Accessibility pass: keyboard navigation works for the main table and filter panel. The export dialog opens by mouse only. Color contrast passed for body text but failed on disabled buttons. The checklist says all interactive controls are keyboard accessible and all text meets contrast."
    , prompt: "Which checklist item should stop first?"
    , options:
        [ { label: "Main table keyboard access", detail: "Supported" }
        , { label: "Filter panel keyboard access", detail: "Supported" }
        , { label: "All controls keyboard accessible", detail: "Dialog mouse-only contradicts it" }
        , { label: "Body text contrast", detail: "Supported" } ]
    , correctIndex: 2
    , rationale: "The export dialog is interactive and not keyboard accessible."
    }
  , {
  domain: "education"
    , source: "Course summary: learners completed four practice quizzes. Scores improved on repeated quiz items, but the final used the same question templates with changed numbers. No transfer task was included. The summary says learners demonstrated general mastery of new problem types."
    , prompt: "Where does the summary go beyond evidence?"
    , options:
        [ { label: "Four quizzes", detail: "Stated directly" }
        , { label: "Repeated-item improvement", detail: "Supported" }
        , { label: "Same templates", detail: "Stated directly" }
        , { label: "General mastery", detail: "No transfer task tested it" } ]
    , correctIndex: 3
    , rationale: "Template-based improvement does not prove general mastery of new problem types."
    }
  , {
  domain: "environmental-monitoring"
    , source: "Sensor memo: station R reported pH every hour except 13:00 to 15:00, when the probe was offline. Readings before and after the gap were within the normal band. The daily bulletin says station R stayed normal all day."
    , prompt: "What is the risky inference?"
    , options:
        [ { label: "Hourly reporting happened", detail: "False during the outage but not the bulletin claim" }
        , { label: "Normal before and after", detail: "Supported" }
        , { label: "Normal all day", detail: "The gap prevents that claim" }
        , { label: "Probe offline", detail: "Stated directly" } ]
    , correctIndex: 2
    , rationale: "The station cannot be described as normal all day when three hours are missing."
    }
  , {
  domain: "product-analytics"
    , source: "Analytics note: 1,200 users saw the new dashboard card. Click-through rose from 8 percent to 11 percent. Session completion stayed at 62 percent. No survey or retention data was collected. The launch recap says users found the card more useful and returned more often."
    , prompt: "Which recap claim is unsupported?"
    , options:
        [ { label: "Click-through rose", detail: "Supported by the note" }
        , { label: "Completion stayed flat", detail: "Supported by the note" }
        , { label: "Users found it useful", detail: "No survey or usefulness measure" }
        , { label: "1,200 users saw it", detail: "Supported by the note" } ]
    , correctIndex: 2
    , rationale: "A click increase alone does not establish perceived usefulness or repeat return."
    }
  , {
  domain: "infrastructure"
    , source: "Capacity report: cluster east ran at 64 percent CPU and 71 percent disk. Cluster west ran at 89 percent CPU and 93 percent disk during batch import. The recommendation says both clusters have comfortable headroom for another import tonight."
    , prompt: "Which recommendation part is weakest?"
    , options:
        [ { label: "East has headroom", detail: "Supported by moderate usage" }
        , { label: "West has comfortable headroom", detail: "High CPU and disk make that doubtful" }
        , { label: "Batch import occurred", detail: "Stated directly" }
        , { label: "Disk was measured", detail: "Stated directly" } ]
    , correctIndex: 1
    , rationale: "West was already near capacity during import."
    }
  , {
  domain: "public-service-admin"
    , source: "Office log: the permit desk processed 36 applications. Seven were returned for missing signatures, and five await inspection reports from another unit. The dashboard note says all permit applications received today were completed by the desk."
    , prompt: "What should be challenged?"
    , options:
        [ { label: "36 applications processed", detail: "Supported" }
        , { label: "Seven returned", detail: "Supported" }
        , { label: "All completed", detail: "Returned and waiting cases remain incomplete" }
        , { label: "Inspection reports external", detail: "Supported" } ]
    , correctIndex: 2
    , rationale: "Processing is not the same as completion when applications were returned or awaiting reports."
    }
  , {
  domain: "editorial-workflow"
    , source: "Editorial board: article M passed copy edit and fact check. The image captions still need source credits. Legal review was marked not required because the article uses synthetic examples. The publication note says article M is ready with all attribution finished."
    , prompt: "Which publication claim is false?"
    , options:
        [ { label: "Copy edit passed", detail: "Supported" }
        , { label: "Fact check passed", detail: "Supported" }
        , { label: "All attribution finished", detail: "Captions still need credits" }
        , { label: "Legal not required", detail: "Stated in the board" } ]
    , correctIndex: 2
    , rationale: "Image caption credits are unfinished attribution work."
    }
  , {
  domain: "support-triage"
    , source: "Support triage: 14 password reset tickets were solved by macro. Six billing tickets were tagged for finance review. Two accessibility tickets are waiting for screenshots. The queue summary says every nontechnical ticket was resolved without escalation."
    , prompt: "Which summary claim should stop?"
    , options:
        [ { label: "Password resets solved", detail: "Supported" }
        , { label: "Billing escalated", detail: "Supported by finance review tag" }
        , { label: "Every nontechnical ticket resolved", detail: "Billing and accessibility remain open" }
        , { label: "Screenshots pending", detail: "Supported" } ]
    , correctIndex: 2
    , rationale: "Several nontechnical tickets still need review or evidence."
    }
  , {
  domain: "manufacturing-qa"
    , source: "QA sheet: lot 27 contains 300 housings. Dimensional checks passed on 292. Four failed width tolerance, and four were set aside because the gauge log was missing. The release label says lot 27 passed all dimensional checks."
    , prompt: "What is the release-label issue?"
    , options:
        [ { label: "Lot size is 300", detail: "Supported" }
        , { label: "292 passed", detail: "Supported" }
        , { label: "All checks passed", detail: "Eight items did not clear checks" }
        , { label: "Gauge log missing", detail: "Supported" } ]
    , correctIndex: 2
    , rationale: "Failures and missing gauge logs prevent an all-passed claim."
    }
  , {
  domain: "incident-review"
    , source: "Incident timeline: alerts fired at 02:10. The cache restart began at 02:18. Error rate fell at 02:24, after a traffic throttle was applied at 02:21. The review draft says the cache restart resolved the incident."
    , prompt: "Which root-cause wording is weak?"
    , options:
        [ { label: "Alerts at 02:10", detail: "Supported" }
        , { label: "Cache restart resolved it", detail: "Throttle happened before recovery" }
        , { label: "Throttle at 02:21", detail: "Supported" }
        , { label: "Errors fell at 02:24", detail: "Supported" } ]
    , correctIndex: 1
    , rationale: "The evidence does not isolate the cache restart as the resolving action."
    }
  , {
  domain: "data-governance"
    , source: "Data catalog: field `region_code` is approved for analytics. Field `free_note` may contain customer-entered text and is restricted from export. The dashboard extract includes both fields. The governance note says the extract uses only approved analytics fields."
    , prompt: "Which field creates the violation?"
    , options:
        [ { label: "region_code", detail: "Approved for analytics" }
        , { label: "free_note", detail: "Restricted from export" }
        , { label: "dashboard extract", detail: "Container, not the field" }
        , { label: "analytics approval", detail: "Applies only to region_code" } ]
    , correctIndex: 1
    , rationale: "`free_note` is restricted from export but appears in the extract."
    }
  , {
  domain: "training-design"
    , source: "Training plan: novices first practice with hints on, then complete a no-hint checkpoint. In the trial, 82 percent passed with hints, but only 46 percent passed the no-hint checkpoint. The summary says hints prepared most novices for independent performance."
    , prompt: "Where is the claim drift?"
    , options:
        [ { label: "82 percent with hints", detail: "Supported" }
        , { label: "46 percent no-hint", detail: "Supported" }
        , { label: "Most prepared independently", detail: "Contradicted by 46 percent" }
        , { label: "Novice sequence", detail: "Supported" } ]
    , correctIndex: 2
    , rationale: "Less than half passed without hints, so most were not ready for independent performance."
    }
  , {
  domain: "procurement"
    , source: "Procurement memo: vendor Q offered the lowest unit price. Vendor R met the delivery date and accessibility packaging requirement. Vendor Q's sample package lacked tactile labels. The recommendation says Q is best because it meets every stated requirement at lowest cost."
    , prompt: "What should be flagged?"
    , options:
        [ { label: "Q lowest price", detail: "Supported" }
        , { label: "R met delivery", detail: "Supported" }
        , { label: "Q meets every requirement", detail: "Tactile labels are missing" }
        , { label: "Packaging sampled", detail: "Supported" } ]
    , correctIndex: 2
    , rationale: "Lowest price does not satisfy a requirement that Q failed."
    }
  , {
  domain: "quality-metrics"
    , source: "Metrics note: reviewer agreement improved from 72 percent to 80 percent after the rubric update. Average review time increased from 6 minutes to 9 minutes. The retrospective says the rubric improved agreement without affecting review speed."
    , prompt: "Which phrase is inaccurate?"
    , options:
        [ { label: "Agreement improved", detail: "Supported" }
        , { label: "After rubric update", detail: "Supported timing" }
        , { label: "Without affecting speed", detail: "Review time increased" }
        , { label: "Average time measured", detail: "Supported" } ]
    , correctIndex: 2
    , rationale: "Review speed was affected because average review time rose."
    }
  , {
  domain: "privacy"
    , source: "Privacy review: survey exports remove names and email addresses. Free-text answers remain included. The prompt asked respondents not to enter private details, but no redaction pass has run. The release checklist says the export is fully anonymized."
    , prompt: "Which checklist claim is unsafe?"
    , options:
        [ { label: "Names removed", detail: "Supported" }
        , { label: "Emails removed", detail: "Supported" }
        , { label: "Fully anonymized", detail: "Free text may still identify people" }
        , { label: "No redaction pass", detail: "Supported" } ]
    , correctIndex: 2
    , rationale: "Removing direct identifiers is not enough when unreviewed free text remains."
    }
  , {
  domain: "documentation"
    , source: "Docs audit: the quickstart covers install, local run, reset, and troubleshooting. It does not explain offline mode or data export. The landing page claim says the documentation covers every user-facing feature in one short guide."
    , prompt: "What is the overclaim?"
    , options:
        [ { label: "Install covered", detail: "Supported" }
        , { label: "Local run covered", detail: "Supported" }
        , { label: "Every feature covered", detail: "Offline mode and export are missing" }
        , { label: "One short guide exists", detail: "Not contradicted" } ]
    , correctIndex: 2
    , rationale: "At least two user-facing features are not documented in the quickstart."
    }
  , {
  domain: "risk-review"
    , source: "Risk register: the migration plan lists rollback tested in staging, backups verified, and access review pending before launch approval. The launch approval note says all high-risk controls are complete except communications, which is low risk."
    , prompt: "Which control status is misstated?"
    , options:
        [ { label: "Rollback tested", detail: "Supported" }
        , { label: "Backups verified", detail: "Supported" }
        , { label: "Access review complete", detail: "It is still pending" }
        , { label: "Communications low risk", detail: "Not enough text to dispute" } ]
    , correctIndex: 2
    , rationale: "The access review is pending, so all high-risk controls are not complete."
    }
  , {
  domain: "analytics-instrumentation"
    , source: "Instrumentation note: the new event fires when a user opens the report panel. It does not fire when the user reads the report or exports it. The metric name is `report_understood`. The dashboard summary says the event measures report comprehension."
    , prompt: "Which measurement claim is invalid?"
    , options:
        [ { label: "Panel opened", detail: "That is what fires" }
        , { label: "Export not tracked", detail: "Supported" }
        , { label: "Comprehension measured", detail: "Opening is not understanding" }
        , { label: "Event has a name", detail: "Supported" } ]
    , correctIndex: 2
    , rationale: "The event measures opening a panel, not reading or comprehension."
    }
  ]

type TraceToken =
  { mark :: String
  , value :: Int
  }

traceRound :: Int -> Int -> Int -> Round
traceRound level seed index =
  let
    size =
      clampInt 4 9 (4 + div level 2 + positiveMod seed 3)

    tokens =
      map (\i -> tokenAt (mixSeed seed (i * 389 + index * 43))) (Array.range 0 (size - 1))

    nBack =
      clampInt 2 (min 5 size) (2 + positiveMod (mixSeed seed 17) 4)

    fromEnd =
      positiveMod (mixSeed seed 29) 2 == 0

    targetIndex =
      if fromEnd then max 0 (size - nBack) else min (size - 1) (nBack - 1)

    target =
      fromMaybe (tokenAt seed) (Array.index tokens targetIndex)

    correct =
      tokenLabel target

    distractors =
      [ tokenLabel (tokenAt (mixSeed seed 101))
      , tokenLabel (tokenAt (mixSeed seed 211))
      , tokenLabel (tokenAt (mixSeed seed 307))
      ]

    correctIndex =
      positiveMod (mixSeed seed level) 4

    promptText =
      if fromEnd then
        "Which token was " <> show nBack <> " from the end?"
      else
        "Which token was " <> show nBack <> " from the start?"

    rationaleText =
      (if fromEnd then "Count backward from the final token as 1. " else "Count forward from the first token as 1. ")
        <> "The target token is "
        <> correct
        <> "."
  in
    { id: "trace-" <> show level <> "-" <> show seed <> "-" <> show index
    , drill: Trace
    , title: "Trace Stack"
    , load: level
    , domain: "sequence"
    , stimulus: String.joinWith "   " (map tokenLabel tokens)
    , prompt: promptText
    , options: placeCorrect correctIndex { label: correct, detail: "Choose from the held sequence" } distractors
    , correctIndex
    , encodeSeconds: clampInt 5 12 (12 - div level 2)
    , answerSeconds: clampInt 6 12 (12 - div level 3)
    , rationale: rationaleText
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
marks = [ "K", "R", "M", "S", "T", "L", "P", "N", "C", "V", "H", "Q", "B", "D", "F", "G", "J", "W" ]

values :: Array Int
values = [ 2, 7, 4, 9, 1, 6, 3, 8, 5, 0, 11, 13, 14, 16, 18, 21, 24, 27, 30, 33 ]

type ReadCase =
  { domain :: String
  , passage :: String
  , prompt :: String
  , options :: Array Option
  , correctIndex :: Int
  , rationale :: String
  }

readRound :: Int -> Int -> Int -> Round
readRound level seed index =
  let
    pickedIndex =
      caseIndex seed index level (Array.length readCases)

    item =
      pick defaultRead readCases pickedIndex

    rotated =
      rotateRoundOptions (mixSeed seed pickedIndex) item.correctIndex item.options
  in
    { id: "read-" <> show level <> "-" <> show pickedIndex <> "-" <> show index
    , drill: Read
    , title: "Dense Read"
    , load: level
    , domain: item.domain
    , stimulus: item.passage
    , prompt: item.prompt
    , options: rotated.options
    , correctIndex: rotated.correctIndex
    , encodeSeconds: clampInt 12 24 (24 - level)
    , answerSeconds: clampInt 8 16 (16 - div level 2)
    , rationale: item.rationale
    }

defaultRead :: ReadCase
defaultRead =
  {
  domain: "software"
    , passage: "The renderer uses a two-step cache. Parsed templates are stored by version, while rendered pages are stored by locale. A version change clears only parsed templates; a locale change clears only rendered pages. Manual flush clears both caches before the next request."
    , prompt: "What does a version change clear?"
    , options:
        [ { label: "Parsed templates only", detail: "Version keys belong to parsed templates" }
        , { label: "Rendered pages only", detail: "Locale changes clear those" }
        , { label: "Both caches", detail: "Only manual flush does this" }
        , { label: "No cache", detail: "A version change has an effect" } ]
    , correctIndex: 0
    , rationale: "The passage separates version-based template invalidation from locale-based page invalidation."
    }

readCases :: Array ReadCase
readCases =
  [ defaultRead
  , {
  domain: "research-methods"
    , passage: "The study has three phases: baseline sorting, guided practice, and delayed retest. Only the retest uses unseen items. The report separates practice gains from transfer by comparing repeated items with unseen items after a two-day delay."
    , prompt: "Which phase uses unseen items?"
    , options:
        [ { label: "Baseline sorting", detail: "Not stated" }
        , { label: "Guided practice", detail: "Uses practice material" }
        , { label: "Delayed retest", detail: "Stated directly" }
        , { label: "Consent screening", detail: "Not one of the phases" } ]
    , correctIndex: 2
    , rationale: "Only the delayed retest uses unseen items."
    }
  , {
  domain: "operations"
    , passage: "The evening handoff has three queues. Queue red must be reviewed before midnight because it contains payment holds. Queue blue can wait until morning. Queue gray is closed only if the audit note and supervisor initials are both present."
    , prompt: "Which queue has the midnight deadline?"
    , options:
        [ { label: "Queue red", detail: "Contains payment holds" }
        , { label: "Queue blue", detail: "Can wait until morning" }
        , { label: "Queue gray", detail: "Depends on two closure items" }
        , { label: "All queues", detail: "Only one has the deadline" } ]
    , correctIndex: 0
    , rationale: "Queue red must be reviewed before midnight."
    }
  , {
  domain: "logistics"
    , passage: "The warehouse splits fragile kits into lane A and bulky kits into lane B. Lane A requires foam inserts before scanning. Lane B requires weight labels after scanning. Mixed kits go to a supervisor shelf and are not loaded until reclassified."
    , prompt: "What happens to mixed kits?"
    , options:
        [ { label: "Loaded into lane A", detail: "Only fragile kits go there" }
        , { label: "Loaded into lane B", detail: "Only bulky kits go there" }
        , { label: "Sent to supervisor shelf", detail: "They wait for reclassification" }
        , { label: "Scanned after weight labels", detail: "That describes lane B order" } ]
    , correctIndex: 2
    , rationale: "Mixed kits are held on a supervisor shelf until reclassified."
    }
  , {
  domain: "finance-budgeting"
    , passage: "The project budget has fixed and flexible pools. Fixed funds cover hosting and audit fees. Flexible funds cover workshops, printing, and travel. Unused flexible funds may move between activities, but fixed funds cannot be moved without a board note."
    , prompt: "Which funds can move between activities?"
    , options:
        [ { label: "Fixed funds", detail: "They cannot move without a board note" }
        , { label: "Flexible funds", detail: "Stated directly" }
        , { label: "Audit fees", detail: "Part of fixed funds" }
        , { label: "Hosting funds", detail: "Part of fixed funds" } ]
    , correctIndex: 1
    , rationale: "Only unused flexible funds may move between activities."
    }
  , {
  domain: "security"
    , passage: "The sandbox grants read access by default but requires a signed request for writes. Temporary write access expires after two hours. Admin review is required only when a write request touches shared configuration or export settings."
    , prompt: "When is admin review required?"
    , options:
        [ { label: "Every read request", detail: "Reads are default allowed" }
        , { label: "Every write request", detail: "Some only need signing" }
        , { label: "Shared config or export writes", detail: "Stated directly" }
        , { label: "After two hours", detail: "That is expiration timing" } ]
    , correctIndex: 2
    , rationale: "Admin review is only for writes touching shared configuration or export settings."
    }
  , {
  domain: "accessibility"
    , passage: "The form uses three focus regions: navigation, data entry, and review. Pressing Escape leaves the current dialog but keeps focus inside the current region. Pressing F6 moves to the next region. Error summaries appear before the data entry fields."
    , prompt: "Which key moves to the next focus region?"
    , options:
        [ { label: "Escape", detail: "Leaves a dialog inside the region" }
        , { label: "F6", detail: "Moves between regions" }
        , { label: "Enter", detail: "Not described" }
        , { label: "Tab", detail: "Not described" } ]
    , correctIndex: 1
    , rationale: "F6 is the region-switching key in the passage."
    }
  , {
  domain: "education"
    , passage: "The lesson plan alternates worked examples and short checks in each topic block. Learners may view hints during examples, but checks hide hints until after submission. The teacher reviews only questions missed twice, not every wrong answer."
    , prompt: "When are hints hidden?"
    , options:
        [ { label: "During worked examples", detail: "Hints may be viewed there" }
        , { label: "During checks until submission", detail: "Stated directly" }
        , { label: "After missed-twice review", detail: "Review timing is separate" }
        , { label: "Always", detail: "Examples allow hints" } ]
    , correctIndex: 1
    , rationale: "Checks hide hints until the learner submits."
    }
  , {
  domain: "environmental-monitoring"
    , passage: "The pond station records turbidity every ten minutes and temperature every hour. A maintenance flag suppresses alerts but does not stop recording. Weekly reports include flagged data only in the appendix, never in the headline chart."
    , prompt: "Where does flagged data appear in weekly reports?"
    , options:
        [ { label: "Headline chart", detail: "Explicitly excluded" }
        , { label: "Appendix", detail: "Stated directly" }
        , { label: "Alert feed", detail: "Alerts are suppressed" }
        , { label: "Nowhere", detail: "It is included in the appendix" } ]
    , correctIndex: 1
    , rationale: "Flagged data is included only in the appendix."
    }
  , {
  domain: "product-analytics"
    , passage: "The experiment tracks three actions: card view, card expand, and report save. A user counts as engaged only after expanding the card or saving the report. Simple views are logged for exposure but are excluded from engagement rate."
    , prompt: "What counts as engagement?"
    , options:
        [ { label: "Simple card view", detail: "Exposure only" }
        , { label: "Expand or save", detail: "Both qualify" }
        , { label: "Report deletion", detail: "Not tracked" }
        , { label: "Any logged action", detail: "Views are excluded" } ]
    , correctIndex: 1
    , rationale: "Engagement requires card expansion or report save."
    }
  , {
  domain: "infrastructure"
    , passage: "The backup system writes snapshots to local disk every hour and copies them to cold storage every night. Restore tests run from cold storage on Fridays. Local snapshots are faster, but Friday tests intentionally use the slower path."
    , prompt: "Which source is used for Friday restore tests?"
    , options:
        [ { label: "Local disk", detail: "Faster but not used for Friday tests" }
        , { label: "Cold storage", detail: "Stated directly" }
        , { label: "Live database", detail: "Not described" }
        , { label: "User export", detail: "Not part of backups" } ]
    , correctIndex: 1
    , rationale: "Friday restore tests run from cold storage."
    }
  , {
  domain: "public-service-admin"
    , passage: "The intake desk sorts requests into urgent, standard, and reference-only bins. Urgent requests need same-day assignment. Standard requests need assignment within three business days. Reference-only requests are logged but not assigned unless the requester asks for follow-up."
    , prompt: "Which bin may be logged without assignment?"
    , options:
        [ { label: "Urgent", detail: "Needs same-day assignment" }
        , { label: "Standard", detail: "Needs assignment within three days" }
        , { label: "Reference-only", detail: "Logged unless follow-up is requested" }
        , { label: "All bins", detail: "Two require assignment" } ]
    , correctIndex: 2
    , rationale: "Reference-only requests are logged and not assigned by default."
    }
  , {
  domain: "editorial-workflow"
    , passage: "A story moves from draft to ready only after structure edit, copy edit, and source review. Art review can happen after ready status, but not after publication. A blocked source review returns the story to draft even if copy edit passed."
    , prompt: "What happens after a blocked source review?"
    , options:
        [ { label: "Story returns to draft", detail: "Stated directly" }
        , { label: "Story becomes ready", detail: "Requires source review to pass" }
        , { label: "Art review is cancelled", detail: "Not stated" }
        , { label: "Publication proceeds", detail: "Not allowed by the workflow" } ]
    , correctIndex: 0
    , rationale: "A blocked source review returns the story to draft."
    }
  , {
  domain: "support-triage"
    , passage: "Triage labels use impact before topic. A login issue blocking many users is high impact. A billing question from one user is normal impact unless money has already moved incorrectly. Accessibility blockers are high impact even with one reporter."
    , prompt: "Which single-reporter issue is high impact by rule?"
    , options:
        [ { label: "Billing question", detail: "Normal unless money moved incorrectly" }
        , { label: "Accessibility blocker", detail: "High impact even with one reporter" }
        , { label: "Topic request", detail: "Not described" }
        , { label: "Routine login question", detail: "Only many-user blocking is specified" } ]
    , correctIndex: 1
    , rationale: "Accessibility blockers are high impact regardless of reporter count."
    }
  , {
  domain: "manufacturing-qa"
    , passage: "Each panel receives visual inspection, torque check, and seal check before packing. Visual inspection happens before assembly. Torque and seal checks happen after assembly. A panel cannot ship unless all three checks have signed entries."
    , prompt: "Which checks happen after assembly?"
    , options:
        [ { label: "Visual only", detail: "Visual happens before assembly" }
        , { label: "Torque and seal", detail: "Stated directly" }
        , { label: "Seal only", detail: "Torque is also after assembly" }
        , { label: "All three", detail: "Visual is before assembly" } ]
    , correctIndex: 1
    , rationale: "Torque and seal checks both happen after assembly."
    }
  , {
  domain: "incident-review"
    , passage: "The incident review separates trigger, amplifier, and recovery action. The trigger started the problem, the amplifier made it worse, and the recovery action reduced user impact. A single item may not be assigned to more than one category in the final timeline."
    , prompt: "What rule applies to final timeline categories?"
    , options:
        [ { label: "One item can fill all categories", detail: "Explicitly disallowed" }
        , { label: "Each item has one category", detail: "Stated directly" }
        , { label: "Only triggers are recorded", detail: "Three categories exist" }
        , { label: "Recovery actions are omitted", detail: "They are included" } ]
    , correctIndex: 1
    , rationale: "The final timeline assigns each item to only one category."
    }
  , {
  domain: "data-governance"
    , passage: "Dataset blue has three zones. Raw keeps original rows for audit. Clean removes duplicate rows but preserves all columns. Share removes restricted columns and keeps only approved derived fields. Analysts may query clean but may export only share."
    , prompt: "Which zone may be exported?"
    , options:
        [ { label: "Raw", detail: "Kept for audit" }
        , { label: "Clean", detail: "Queryable but not exportable" }
        , { label: "Share", detail: "The only export zone" }
        , { label: "All zones", detail: "Only share may be exported" } ]
    , correctIndex: 2
    , rationale: "Analysts may export only the share zone."
    }
  , {
  domain: "training-design"
    , passage: "The drill ladder has recall, compare, and audit stages. Recall asks for facts from memory. Compare asks for differences between two notes. Audit asks whether a summary should be blocked. Learners unlock audit only after two clean compare rounds."
    , prompt: "What unlocks audit?"
    , options:
        [ { label: "One recall round", detail: "Not enough" }
        , { label: "Two clean compare rounds", detail: "Stated directly" }
        , { label: "Any blocked summary", detail: "That is part of audit" }
        , { label: "Reading two notes", detail: "Not the unlock rule" } ]
    , correctIndex: 1
    , rationale: "Audit unlocks after two clean compare rounds."
    }
  , {
  domain: "procurement"
    , passage: "The bid review scores price, delivery, and service coverage for each proposal. Price carries 40 points, delivery 30, and service coverage 30. A bid cannot win if service coverage is zero, even with a perfect price score."
    , prompt: "Which zero score blocks a bid from winning?"
    , options:
        [ { label: "Price", detail: "Important but not the blocking rule" }
        , { label: "Delivery", detail: "No zero-block rule stated" }
        , { label: "Service coverage", detail: "Stated directly" }
        , { label: "Total score", detail: "Not a category" } ]
    , correctIndex: 2
    , rationale: "Service coverage of zero blocks a bid from winning."
    }
  , {
  domain: "quality-metrics"
    , passage: "The dashboard shows defect count, review delay, and rework rate. Defect count is raw volume. Rework rate is the share of completed items reopened within seven days. Review delay is measured from submission to first reviewer action."
    , prompt: "How is rework rate defined?"
    , options:
        [ { label: "Raw defect volume", detail: "That is defect count" }
        , { label: "Reopened completed items share", detail: "Stated directly" }
        , { label: "Submission to reviewer action", detail: "That is review delay" }
        , { label: "All delayed reviews", detail: "Not the definition" } ]
    , correctIndex: 1
    , rationale: "Rework rate is the share of completed items reopened within seven days."
    }
  , {
  domain: "privacy"
    , passage: "The export tool has summary and detail modes. Summary mode groups responses by team and drops comments. Detail mode keeps comments but masks direct identifiers. Detail mode requires a reviewer note before the file can be downloaded."
    , prompt: "Which export mode requires a reviewer note?"
    , options:
        [ { label: "Summary mode", detail: "Groups responses and drops comments" }
        , { label: "Detail mode", detail: "Reviewer note required" }
        , { label: "Both modes", detail: "Only detail is specified" }
        , { label: "Neither mode", detail: "Detail has a requirement" } ]
    , correctIndex: 1
    , rationale: "Detail mode requires a reviewer note before download."
    }
  , {
  domain: "documentation"
    , passage: "The release guide has three paths for maintainers. Quick repair covers one-file patches. Standard release covers reviewed feature work. Emergency release covers urgent fixes and requires a follow-up review note by the next business day."
    , prompt: "Which path needs a follow-up review note?"
    , options:
        [ { label: "Quick repair", detail: "No follow-up note stated" }
        , { label: "Standard release", detail: "Reviewed before release" }
        , { label: "Emergency release", detail: "Requires next-day follow-up" }
        , { label: "All paths", detail: "Only emergency is specified" } ]
    , correctIndex: 2
    , rationale: "Emergency releases require a follow-up review note by the next business day."
    }
  , {
  domain: "risk-review"
    , passage: "The risk board uses watch, hold, and block states. Watch means continue with monitoring. Hold means pause until one named condition is met. Block means stop until the board records a new decision. Hold is not the same as block."
    , prompt: "What does hold mean?"
    , options:
        [ { label: "Continue with monitoring", detail: "That is watch" }
        , { label: "Pause until one condition is met", detail: "Stated directly" }
        , { label: "Stop until a new board decision", detail: "That is block" }
        , { label: "Proceed without review", detail: "Not stated" } ]
    , correctIndex: 1
    , rationale: "Hold pauses work until a named condition is met."
    }
  , {
  domain: "analytics-instrumentation"
    , passage: "The event schema has actor, action, object, and context fields. Actor and object are required. Context is optional and may contain screen size or experiment bucket. Action must be one of open, save, dismiss, or retry."
    , prompt: "Which field is optional?"
    , options:
        [ { label: "actor", detail: "Required" }
        , { label: "object", detail: "Required" }
        , { label: "context", detail: "Optional" }
        , { label: "action", detail: "Required with limited values" } ]
    , correctIndex: 2
    , rationale: "Context is optional; actor, object, and action are constrained."
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
