module Main (main) where

import Data.Time
import Data.Text
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Pomodoro" $ do
    return ()
