module Main (main) where

import Data.Time.Clock.POSIX (POSIXTime)
import Network.HTTP.Client hiding (Proxy)
import Network.Wai.Handler.Warp (testWithApplication)
import Pomodoro.Domain
import Pomodoro.Web
import Servant
import Servant.Client
import Servant.Client.Generic (genericClient)
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
    around (testWithApplication (app <$> mkEnv)) $
      describe "REST API" $ do
        it "when reading health endpoint, it should get a 200" $ \port -> do
          clientEnv <- makeClientEnv port
          result <- runClientM ((.health) clientRoutes) clientEnv
          result `shouldBe` Right NoContent
        it "when starting at 0 asking at 5 minutes on a classical sequence, it should answer 20 minutes left" $ \port -> do
          clientEnv <- makeClientEnv port
          Right () <- runClientM ((.start) clientRoutes (StartArgs {eventLog = base $ minutes 0})) clientEnv
          result <- runClientM ((.status) clientRoutes (StatusArgs {time = minutes 5})) clientEnv
          result `shouldBe` Right (Remaining Work (minutes 20))
        it "when starting at 0 asking at 15 minutes on a classical sequence, it should answer 10 minutes left" $ \port -> do
          clientEnv <- makeClientEnv port
          Right () <- runClientM ((.start) clientRoutes (StartArgs {eventLog = base $ minutes 0})) clientEnv
          result <- runClientM ((.status) clientRoutes (StatusArgs {time = minutes 15})) clientEnv
          result `shouldBe` Right (Remaining Work (minutes 10))
        it "when starting at 0 asking at 28 minutes on a classical sequence, it should answer 2 minutes left" $ \port -> do
          clientEnv <- makeClientEnv port
          Right () <- runClientM ((.start) clientRoutes (StartArgs {eventLog = base (minutes 0) <> [StartedBreak (minutes 25)]})) clientEnv
          result <- runClientM ((.status) clientRoutes (StatusArgs {time = minutes 28})) clientEnv
          result `shouldBe` Right (Remaining Break (minutes 2))
        it "when starting at 0 asking at 20 and 28 minutes on a classical sequence, it should answer 5 and 2 minutes left" $ \port -> do
          clientEnv <- makeClientEnv port
          Right () <- runClientM ((.start) clientRoutes (StartArgs {eventLog = base (minutes 0)})) clientEnv
          result0 <- runClientM ((.status) clientRoutes (StatusArgs {time = minutes 20})) clientEnv
          result0 `shouldBe` Right (Remaining Work (minutes 5))
          Right () <- runClientM ((.update) clientRoutes (StartArgs {eventLog = base (minutes 0) <> [StartedBreak (minutes 25)]})) clientEnv
          result1 <- runClientM ((.status) clientRoutes (StatusArgs {time = minutes 28})) clientEnv
          result1 `shouldBe` Right (Remaining Break (minutes 2))
        it "when client lost event log, can ask for it" $ \port -> do
          clientEnv <- makeClientEnv port
          Right () <- runClientM ((.start) clientRoutes (StartArgs {eventLog = base (minutes 0)})) clientEnv
          result0 <- runClientM ((.status) clientRoutes (StatusArgs {time = minutes 20})) clientEnv
          result0 `shouldBe` Right (Remaining Work (minutes 5))
          eventLog <- runClientM ((.events) clientRoutes) clientEnv
          eventLog `shouldBe` Right (base $ minutes 0)

makeClientEnv :: Int -> IO ClientEnv
makeClientEnv port = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  pure $ mkClientEnv manager baseUrl

base :: POSIXTime -> [Event]
base start =
  [StartedWork start]

clientRoutes :: Routes (AsClientT ClientM)
clientRoutes = genericClient
