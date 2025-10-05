module Main (main) where

import Data.Time
-- import Data.Text

import Data.Time.Clock.POSIX (POSIXTime)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Pomodoro" $ do
    describe "sequenceStatus" $ do
      it "when started at 0 asking at 5 minutes on a classical sequence, it should answer 20 minutes left" $
        sequenceStatus (base $ minutes 0) (minutes 5) `shouldBe` Remaining (minutes 20)
      it "when started at 0 asking at 15 minutes on a classical sequence, it should answer 10 minutes left" $
        sequenceStatus (base $ minutes 0) (minutes 15) `shouldBe` Remaining (minutes 10)
      it "when started at 0 asking at 30 minutes on a classical sequence, it should answer 5 minutes overtime" $
        sequenceStatus (base $ minutes 0) (minutes 30) `shouldBe` Overtime (minutes 5)
      it "when started at 0 asking at 30 minutes on a classical sequence, it should answer 5 minutes overtime" $
        sequenceStatus (base $ minutes 0) (minutes 30) `shouldBe` Overtime (minutes 5)
      it "when started at 10 minutes asking at 40 minutes on a classical sequence, it should answer 5 minutes overtime" $
        sequenceStatus (base $ minutes 10) (minutes 40) `shouldBe` Overtime (minutes 5)
      it "when started the break at 30 minutes asking at 34 minutes on a classical sequence, it should answer 1 minutes left" $
        sequenceStatus (base (minutes 0) <> [StartedBreak (minutes 30)]) (minutes 34) `shouldBe` Remaining (minutes 1)

base :: POSIXTime -> [Event]
base start =
  [ Configured Config {pomodoro = minutes 25, shortPause = minutes 5, longPause = minutes 15},
    StartedWork start
  ]

minutes :: Int -> POSIXTime
minutes = secondsToNominalDiffTime . (* 60) . fromIntegral

data SequenceStatus = Remaining POSIXTime | Overtime POSIXTime
  deriving stock (Eq, Show)

data Config = Config {pomodoro, shortPause, longPause :: POSIXTime}

data Event = Configured Config | StartedWork POSIXTime | StartedBreak POSIXTime

sequenceStatus :: [Event] -> POSIXTime -> SequenceStatus
sequenceStatus (Configured config:starts) queriedAt =
  if delta >= 0
    then
      Remaining delta
    else
      Overtime $ -delta
  where
    delta = phaseDuration + begin - queriedAt
    (begin, phaseDuration) =
      case last starts of
        StartedWork x -> (x, config.pomodoro)
        StartedBreak x -> (x, config.shortPause)
