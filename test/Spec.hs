{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Pomodoro.Domain
import Servant
import Servant.Client
import Servant.Client.Generic (genericClient)
import Servant.Server.Generic (AsServer, genericServe)
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
    around (testWithApplication (pure app)) $
      describe "REST API" $ do
        it "when reading health endpoint, it should get a 200" $ \port -> do
          clientEnv <- makeClientEnv port
          result <- runClientM (health clientRoutes) clientEnv
          result `shouldBe` Right NoContent
        it "when starting at 0 asking at 5 minutes on a classical sequence, it should answer 20 minutes left" $ \port -> do
          clientEnv <- makeClientEnv port
          seqId' <- runClientM (start clientRoutes (StartArgs{eventLog = base $ minutes 0})) clientEnv
          seqId' `shouldBe` Right (SequenceId 1)
          result <- runClientM (status clientRoutes (StatusArgs{time = minutes 5})) clientEnv
          result `shouldBe` Right (Remaining Work (minutes 20))

makeClientEnv :: Int -> IO ClientEnv
makeClientEnv port = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  pure $ mkClientEnv manager baseUrl

base :: POSIXTime -> [Event]
base start =
  [StartedWork start]

sequenceStatus' :: [Event] -> POSIXTime -> SequenceStatus
sequenceStatus' = sequenceStatus classicalConfig

data Routes route = Routes
  { health :: route :- "health" :> Get '[JSON] NoContent
  , start :: route :- "start" :> ReqBody '[JSON] StartArgs :> Post '[JSON] SequenceId
  , status :: route :- "status" :> ReqBody '[JSON] StatusArgs :> Post '[JSON] SequenceStatus
  }
  deriving (Generic)

newtype StartArgs = StartArgs {eventLog :: [Event]}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype SequenceId = SequenceId {seqId :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype StatusArgs
  = StatusArgs {time :: POSIXTime}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

healthServer :: Routes AsServer
healthServer =
  Routes
    { health = pure NoContent
    , start = const $ pure $ SequenceId 1
    , status = const $ pure $ Remaining Work (minutes 20)
    }

app :: Application
app = genericServe healthServer

clientRoutes :: Routes (AsClientT ClientM)
clientRoutes = genericClient
