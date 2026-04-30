module SignalGym.Random
  ( randomSeed
  ) where

import Effect (Effect)

foreign import randomSeed :: Effect Int
