{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Data.Time
import Data.Time.Clock.POSIX (POSIXTime)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Pomodoro" $ do
    describe "sequenceStatus" $ do
      it "when started at 0 asking at 5 minutes on a classical sequence, it should answer 20 minutes left" $
        sequenceStatus' (base $ minutes 0) (minutes 5)
          `shouldBe` Remaining Work (minutes 20)
      it "when started at 0 asking at 15 minutes on a classical sequence, it should answer 10 minutes left" $
        sequenceStatus' (base $ minutes 0) (minutes 15)
          `shouldBe` Remaining Work (minutes 10)
      it "when started at 0 asking at 30 minutes on a classical sequence, it should answer 5 minutes overtime" $
        sequenceStatus' (base $ minutes 0) (minutes 30)
          `shouldBe` Overtime Work (minutes 5)
      it "when started at 10 minutes asking at 40 minutes on a classical sequence, it should answer 5 minutes overtime" $
        sequenceStatus' (base $ minutes 10) (minutes 40)
          `shouldBe` Overtime Work (minutes 5)
      it "when started the break at 30 minutes asking at 34 minutes on a classical sequence, it should answer 1 minutes left" $
        sequenceStatus' (base (minutes 0) <> [StartedBreak (minutes 30)]) (minutes 34)
          `shouldBe` Remaining Break (minutes 1)
      it "when starting a second work period at 32, at 50 minutes should remain 7 minutes" $
        sequenceStatus' (base (minutes 0) <> [StartedBreak (minutes 26), StartedWork (minutes 32)]) (minutes 50)
          `shouldBe` Remaining Work (minutes 7)
      it "when starting a second work period at 32, at 50 minutes should remain 7 minutes" $
        sequenceStatus' (base (minutes 0) <> [StartedBreak (minutes 26), StartedWork (minutes 32), StartedBreak (minutes 57), StartedWork (minutes 63), StartedBreak (minutes 89), StartedWork (minutes 95), StartedBreak (minutes 121)]) (minutes 122)
          `shouldBe` Remaining Break (minutes 14)

base :: POSIXTime -> [Event]
base start =
  [StartedWork start]

minutes :: Int -> POSIXTime
minutes = secondsToNominalDiffTime . (* 60) . fromIntegral

data SequenceStatus = Remaining Stage POSIXTime | Overtime Stage POSIXTime
  deriving stock (Eq, Show)

data Stage = Work | Break
  deriving stock (Eq, Show)

data Config = Config {pomodoro, shortPause, longPause :: POSIXTime}

classicalConfig :: Config
classicalConfig = Config{pomodoro = minutes 25, shortPause = minutes 5, longPause = minutes 15}

data Event = StartedWork POSIXTime | StartedBreak POSIXTime

sequenceStatus' = sequenceStatus classicalConfig

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
