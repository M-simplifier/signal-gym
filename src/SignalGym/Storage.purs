module SignalGym.Storage
  ( loadProfile
  , saveProfile
  ) where

import Effect (Effect)
import Prelude (Unit)
import SignalGym.Training (Profile)

foreign import loadProfile :: Effect Profile

foreign import saveProfile :: Profile -> Effect Unit
