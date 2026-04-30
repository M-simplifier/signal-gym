module SignalGym.Main where

import Prelude

import Asterism (start)
import Effect (Effect)
import SignalGym.App (choreography)

main :: Effect Unit
main =
  start "#app" choreography
