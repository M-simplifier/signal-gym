module SignalGym.App
  ( Model
  , Msg(..)
  , choreography
  ) where

import Prelude hiding (div)

import Asterism
  ( Choreography
  , Html
  , Phase
  , Signal
  , ariaPressed
  , attribute
  , button
  , class_
  , data_
  , disabled
  , div
  , every
  , exec
  , fragment
  , h1
  , h2
  , header
  , key
  , main_
  , node
  , onClick
  , p
  , perform
  , quiet
  , role
  , section
  , small
  , span
  , style
  , text
  , withBursts
  )
import Data.Array as Array
import Data.Maybe (Maybe(..))
import SignalGym.Storage (loadProfile, saveProfile)
import SignalGym.Training as Training
import SignalGym.Training (Mode(..), Profile, Session, Stage(..))

type Model =
  { profile :: Profile
  , loaded :: Boolean
  , session :: Maybe Session
  }

data Msg
  = Loaded Profile
  | Start Mode
  | Tick
  | HideStimulus
  | Pick Int
  | NextRound
  | CloseSession

initialModel :: Model
initialModel =
  { profile: Training.emptyProfile
  , loaded: false
  , session: Nothing
  }

choreography :: Choreography Model Msg
choreography =
  { boot: withBursts [ perform (Loaded <$> loadProfile) ] (quiet initialModel)
  , evolve
  , render
  , signals
  }

signals :: Model -> Array (Signal Msg)
signals model =
  case model.session of
    Just session
      | session.stage == Encoding || session.stage == Answering ->
          [ every "session-clock" 1000 Tick ]

    _ ->
      []

evolve :: Msg -> Model -> Phase Model Msg
evolve msg model =
  case msg of
    Loaded profile ->
      quiet (model { profile = profile, loaded = true })

    Start mode ->
      quiet (model { session = Just (Training.startSession model.profile mode) })

    Tick ->
      quiet (model { session = map Training.tickSession model.session })

    HideStimulus ->
      quiet (model { session = map Training.revealCurrent model.session })

    Pick index ->
      quiet (model { session = map (Training.answerCurrent (Just index)) model.session })

    NextRound ->
      case model.session of
        Nothing ->
          quiet model

        Just session ->
          let
            advanced =
              Training.advanceAfterFeedback session
          in
            if advanced.stage == Complete then
              let
                nextProfile =
                  Training.completeProfile model.profile advanced
              in
                withBursts [ exec (saveProfile nextProfile) ]
                  (quiet (model { profile = nextProfile, session = Just advanced }))
            else
              quiet (model { session = Just advanced })

    CloseSession ->
      quiet (model { session = Nothing })

render :: Model -> Html Msg
render model =
  main_ [ class_ "app-shell" ]
    [ appHeader model
    , case model.session of
        Nothing ->
          dashboard model

        Just session ->
          sessionView model.profile session
    ]

appHeader :: Model -> Html Msg
appHeader model =
  header [ class_ "topbar" ]
    [ div [ class_ "brand-lockup" ]
        [ node "img" [ class_ "brand-mark", data_ "loaded" (if model.loaded then "yes" else "no"), attribute "src" "mark.svg", attribute "alt" "" ] []
        , div []
            [ h1 [] [ text "Signal Gym" ]
            , p [] [ text "Fast practice for AI-era document work" ]
            ]
        ]
    , div [ class_ "top-metrics" ]
        [ metric "streak" (show model.profile.streak)
        , metric "xp" (show model.profile.xp)
        , metric "best" (show model.profile.bestScore)
        ]
    ]

dashboard :: Model -> Html Msg
dashboard model =
  section [ class_ "dashboard" ]
    [ div [ class_ "mode-grid" ]
        [ modeCard DailyMix "Daily Mix" "3 drills / 12 rounds" "mixed" model.profile
        , modeCard GateOnly "Gate" "claim drift" "gate" model.profile
        , modeCard TraceOnly "Trace" "token stack" "trace" model.profile
        , modeCard ReadOnly "Read" "dense recall" "read" model.profile
        ]
    , section [ class_ "status-band" ]
        [ profilePanel model.profile
        , calibrationPanel model.profile
        ]
    , section [ class_ "evidence-strip" ]
        [ div [] [ span [] [ text "claim" ], strongText "near-task training" ]
        , div [] [ span [] [ text "scope" ], strongText "local only" ]
        , div [] [ span [] [ text "guard" ], strongText "no medical claim" ]
        ]
    ]

modeCard :: Mode -> String -> String -> String -> Profile -> Html Msg
modeCard mode titleText subtitle theme profile =
  button
    [ class_ ("mode-card " <> theme)
    , onClick (Start mode)
    , ariaPressed false
    ]
    [ span [ class_ "mode-kicker" ] [ text (Training.modeLabel mode) ]
    , span [ class_ "mode-title" ] [ text titleText ]
    , span [ class_ "mode-subtitle" ] [ text subtitle ]
    , span [ class_ "mode-level" ] [ text (modeLevel mode profile) ]
    ]

profilePanel :: Profile -> Html Msg
profilePanel profile =
  div [ class_ "panel profile-panel" ]
    [ h2 [] [ text "Today" ]
    , div [ class_ "profile-grid" ]
        [ statCell "sessions" (show profile.sessions)
        , statCell "focus min" (show profile.focusMinutes)
        , statCell "best run" (show profile.bestScore)
        , statCell "streak" (show profile.streak)
        ]
    ]

calibrationPanel :: Profile -> Html Msg
calibrationPanel profile =
  div [ class_ "panel calibration-panel" ]
    [ h2 [] [ text "Calibration" ]
    , levelRow "Gate" profile.gateLevel "gate"
    , levelRow "Trace" profile.traceLevel "trace"
    , levelRow "Read" profile.readLevel "read"
    ]

sessionView :: Profile -> Session -> Html Msg
sessionView profile session =
  if session.stage == Complete then
    completeView profile session
  else
    case Training.currentRound session of
      Nothing ->
        completeView profile session

      Just round ->
        section [ class_ ("session-surface " <> Training.drillClass round.drill) ]
          [ div [ class_ "session-head" ]
              [ div []
                  [ span [ class_ "mode-kicker" ] [ text (Training.drillLabel round.drill) ]
                  , h2 [] [ text round.title ]
                  ]
              , div [ class_ "session-score" ]
                  [ span [] [ text ("R" <> show (min (Training.roundCount session) (session.index + 1)) <> "/" <> show (Training.roundCount session)) ]
                  , span [] [ text ("score " <> show session.score) ]
                  , span [] [ text ("combo " <> show session.combo) ]
                  ]
              ]
          , progressRail session
          , div [ class_ "arena" ]
              [ timerBlock session
              , stimulusBlock session round
              , promptBlock session round
              ]
          , sessionFooter session
          ]

stimulusBlock :: Session -> Training.Round -> Html Msg
stimulusBlock session round =
  div [ class_ ("stimulus " <> (if session.stage == Encoding then "open" else "closed")) ]
    [ div [ class_ "stimulus-head" ]
        [ span [] [ text (Training.levelLabel round.load) ]
        , span [] [ text (stageLabel session.stage) ]
        ]
    , if session.stage == Encoding then
        p [] [ text round.stimulus ]
      else
        div [ class_ "memory-mask" ]
          [ span [] [ text "encoded" ]
          , span [] [ text "######" ]
          ]
    , if session.stage == Encoding then
        button [ class_ "primary-action", onClick HideStimulus ] [ text "Hide and answer" ]
      else
        fragment []
    ]

promptBlock :: Session -> Training.Round -> Html Msg
promptBlock session round =
  div [ class_ "prompt-panel" ]
    [ p [ class_ "prompt" ] [ text round.prompt ]
    , div [ class_ "option-grid", role "list" ]
        (Array.mapWithIndex (optionButton session round) round.options)
    , feedbackBlock session
    ]

optionButton :: Session -> Training.Round -> Int -> Training.Option -> Html Msg
optionButton session round index option =
  let
    answered =
      session.stage == Feedback

    isCorrect =
      answered && index == round.correctIndex

    wasPicked =
      case session.feedback of
        Just feedback -> feedback.selected == Just index
        Nothing -> false

    stateClass =
      if isCorrect then " correct"
      else if wasPicked then " picked"
      else ""
  in
    button
      [ key ("option-" <> show index)
      , class_ ("option-button" <> stateClass)
      , disabled (session.stage /= Answering)
      , onClick (Pick index)
      ]
      [ span [ class_ "option-label" ] [ text option.label ]
      , small [] [ text option.detail ]
      ]

feedbackBlock :: Session -> Html Msg
feedbackBlock session =
  case session.feedback of
    Nothing ->
      fragment []

    Just feedback ->
      div [ class_ ("feedback " <> (if feedback.correct then "good" else "bad")) ]
        [ div []
            [ strongText feedback.label
            , span [] [ text (if feedback.gain > 0 then " +" <> show feedback.gain else " +0") ]
            ]
        , p [] [ text feedback.rationale ]
        , button [ class_ "primary-action compact", onClick NextRound ] [ text "Next" ]
        ]

completeView :: Profile -> Session -> Html Msg
completeView profile session =
  section [ class_ "complete-surface" ]
    [ div [ class_ "complete-card" ]
        [ span [ class_ "mode-kicker" ] [ text "complete" ]
        , h2 [] [ text (Training.modeLabel session.mode <> " finished") ]
        , div [ class_ "complete-stats" ]
            [ statCell "score" (show session.score)
            , statCell "accuracy" (show (Training.sessionAccuracy session) <> "%")
            , statCell "streak" (show profile.streak)
            , statCell "xp" (show profile.xp)
            ]
        , div [ class_ "complete-actions" ]
            [ button [ class_ "primary-action", onClick (Start session.mode) ] [ text "Run again" ]
            , button [ class_ "secondary-action", onClick CloseSession ] [ text "Back" ]
            ]
        ]
    ]

timerBlock :: Session -> Html Msg
timerBlock session =
  div [ class_ "timer-block" ]
    [ span [ class_ "timer-value" ] [ text (show session.remaining) ]
    , small [] [ text (stageLabel session.stage) ]
    ]

progressRail :: Session -> Html Msg
progressRail session =
  let
    done =
      min (Training.roundCount session) session.index

    total =
      max 1 (Training.roundCount session)

    pct =
      (done * 100) / total
  in
    div [ class_ "progress-rail" ]
      [ div [ class_ "progress-fill", style "width" (show pct <> "%") ] [] ]

sessionFooter :: Session -> Html Msg
sessionFooter session =
  div [ class_ "session-footer" ]
    [ span [] [ text ("accuracy " <> show (Training.sessionAccuracy session) <> "%") ]
    , span [] [ text ("answered " <> show session.answered) ]
    , button [ class_ "ghost-action", onClick CloseSession ] [ text "Exit" ]
    ]

metric :: String -> String -> Html Msg
metric label value =
  div [ class_ "metric" ]
    [ span [] [ text label ]
    , strongText value
    ]

statCell :: String -> String -> Html Msg
statCell label value =
  div [ class_ "stat-cell" ]
    [ span [] [ text label ]
    , strongText value
    ]

levelRow :: String -> Int -> String -> Html Msg
levelRow label value theme =
  div [ class_ ("level-row " <> theme) ]
    [ span [] [ text label ]
    , div [ class_ "level-track" ]
        [ div [ class_ "level-fill", style "width" (show (value * 11) <> "%") ] [] ]
    , strongText (Training.levelLabel value)
    ]

modeLevel :: Mode -> Profile -> String
modeLevel mode profile = case mode of
  DailyMix ->
    "Lv." <> show ((profile.gateLevel + profile.traceLevel + profile.readLevel) / 3)

  GateOnly ->
    Training.levelLabel profile.gateLevel

  TraceOnly ->
    Training.levelLabel profile.traceLevel

  ReadOnly ->
    Training.levelLabel profile.readLevel

stageLabel :: Stage -> String
stageLabel stage = case stage of
  Encoding -> "encode"
  Answering -> "answer"
  Feedback -> "feedback"
  Complete -> "complete"

strongText :: String -> Html Msg
strongText value =
  node "strong" [] [ text value ]
