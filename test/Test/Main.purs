module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Assert (assert)

import SignalGym.Training as Training
import SignalGym.Training (Mode(..), Stage(..))

main :: Effect Unit
main = do
  testSessionStarts
  testSeededSessionsVary
  testCorrectAnswerScores
  testProfileCompletion

testSessionStarts :: Effect Unit
testSessionStarts = do
  let
    session =
      Training.startSession Training.emptyProfile DailyMix

  assert (Training.roundCount session == 12)
  assert (session.stage == Encoding)

testSeededSessionsVary :: Effect Unit
testSeededSessionsVary = do
  let
    first =
      Training.startSessionWithSeed Training.emptyProfile GateOnly 111

    second =
      Training.startSessionWithSeed Training.emptyProfile GateOnly 222

  case Training.currentRound first, Training.currentRound second of
    Just a, Just b -> do
      assert (a.id /= b.id)
      assert (a.domain /= "")
      assert (b.domain /= "")

    _, _ ->
      assert false

testCorrectAnswerScores :: Effect Unit
testCorrectAnswerScores = do
  let
    started =
      Training.startSession Training.emptyProfile GateOnly

    answering =
      Training.revealCurrent started

  case Training.currentRound answering of
    Nothing ->
      assert false

    Just round -> do
      let
        answered =
          Training.answerCurrent (Just round.correctIndex) answering

      assert (answered.stage == Feedback)
      assert (answered.correct == 1)
      assert (answered.score > 0)

testProfileCompletion :: Effect Unit
testProfileCompletion = do
  let
    profile =
      Training.emptyProfile
        { todayKey = "2026-04-30"
        , yesterdayKey = "2026-04-29"
        , lastDay = "2026-04-29"
        , streak = 2
        }

    session =
      finishPerfect (Training.startSession profile TraceOnly)

    next =
      Training.completeProfile profile session

  assert (next.streak == 3)
  assert (next.sessions == 1)
  assert (next.traceLevel == profile.traceLevel + 1)

finishPerfect :: Training.Session -> Training.Session
finishPerfect session =
  case Training.currentRound session of
    Nothing ->
      session

    Just round ->
      let
        answered =
          Training.answerCurrent (Just round.correctIndex) (Training.revealCurrent session)

        next =
          Training.advanceAfterFeedback answered
      in
        if next.stage == Complete then next else finishPerfect next
