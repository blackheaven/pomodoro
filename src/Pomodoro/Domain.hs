module Pomodoro.Domain
  ( sequenceStatus,
    Config (..),
    classicalConfig,
    Event (..),
    SequenceStatus (..),
    Stage (..),
    minutes,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

minutes :: Int -> POSIXTime
minutes = secondsToNominalDiffTime . (* 60) . fromIntegral

data SequenceStatus = Remaining Stage POSIXTime | Overtime Stage POSIXTime
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Stage = Work | Break
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Config = Config {pomodoro, shortPause, longPause :: POSIXTime}

classicalConfig :: Config
classicalConfig = Config {pomodoro = minutes 25, shortPause = minutes 5, longPause = minutes 15}

data Event = StartedWork POSIXTime | StartedBreak POSIXTime
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

sequenceStatus :: Config -> [Event] -> POSIXTime -> SequenceStatus
sequenceStatus config starts queriedAt =
  if delta >= 0
    then
      Remaining currentStage delta
    else
      Overtime currentStage $ -delta
  where
    delta = phaseDuration + begin - queriedAt
    (begin, phaseDuration) =
      case last starts of
        StartedWork x -> (x, config.pomodoro)
        StartedBreak x -> (x, if isLongPause then config.longPause else config.shortPause)
    isLongPause = length (filter isWorkEvent starts) `mod` 4 == 0
    currentStage = if isWorkEvent (last starts) then Work else Break
    isWorkEvent =
      \case
        StartedWork _ -> True
        StartedBreak _ -> False
