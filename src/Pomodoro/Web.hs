module Pomodoro.Web where

import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics
import Network.Wai
import Pomodoro.Domain
import Servant
import Servant.Server.Generic (AsServerT, genericServeT)

data Routes route = Routes
  { health :: route :- "health" :> Get '[JSON] NoContent,
    start :: route :- "start" :> ReqBody '[JSON] StartArgs :> Post '[JSON] (),
    update :: route :- "update" :> ReqBody '[JSON] StartArgs :> Post '[JSON] (),
    status :: route :- "status" :> ReqBody '[JSON] StatusArgs :> Post '[JSON] SequenceStatus,
    events :: route :- "events" :> Get '[JSON] [Event]
  }
  deriving (Generic)

newtype StartArgs = StartArgs {eventLog :: [Event]}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype StatusArgs
  = StatusArgs {time :: POSIXTime}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

healthServer :: Routes (AsServerT (AppM Handler))
healthServer =
  Routes
    { health = pure NoContent,
      start = \body -> do
        eventLogRef <- asks (.eventStore)
        liftIO $ writeIORef eventLogRef body.eventLog,
      update = \body -> do
        eventLogRef <- asks (.eventStore)
        liftIO $ writeIORef eventLogRef body.eventLog,
      status = \body -> do
        eventLogRef <- asks (.eventStore)
        events <- liftIO $ readIORef eventLogRef
        pure $ sequenceStatus' events body.time,
      events = do
        eventLogRef <- asks (.eventStore)
        liftIO $ readIORef eventLogRef
    }

app :: Env -> Application
app env = genericServeT (flip runReaderT env . (.unAppM)) healthServer

newtype AppM m a
  = AppM {unAppM :: ReaderT Env m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

newtype Env
  = Env {eventStore :: IORef [Event]}

mkEnv :: IO Env
mkEnv = Env <$> newIORef mempty
