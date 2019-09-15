{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}


module Main where


import Conduit
  ( (.|)
  , linesUnboundedAsciiC
  , mapM_C
  , mapOutputMaybe
  , runConduit
  , sourceHandle
  )
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TQueue
  , TVar
  , atomically
  , modifyTVar'
  , newTQueueIO
  , newTVarIO
  , readTQueue
  , readTVar
  , writeTQueue
  )
import Control.Monad.Trans (liftIO)
import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSONKey
  , (.:)
  , decodeStrict
  , withObject
  )
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setPort
  )
import Prelude hiding (Word)
import Servant ((:<|>)((:<|>)), (:>), Get, JSON, Proxy(Proxy), Server, serve)
import System.IO (Handle, stdin)


newtype Word =
  Word Text
  deriving newtype (Eq, Ord, ToJSONKey)


newtype EventType =
  EventType Text
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSONKey)


data Event =
  Event
    { eventType      :: !EventType
    , eventData      :: !Text
    , eventTimestamp :: !Int
    }
  deriving stock Show


instance FromJSON Event where
  parseJSON =
    withObject "Event" $ \v -> do
      eventType      <- v .: "event_type"
      eventData      <- v .: "data"
      eventTimestamp <- v .: "timestamp"
      pure Event {..}


type EventCount = Map EventType Int
type WordCount  = Map Word      Int


data Stats =
  Stats
    { statsEvents :: !EventCount
    , statsWords  :: !WordCount
    }


instance Semigroup Stats where
  Stats e w <> Stats e' w' =
    Stats (Map.unionWith (+) e e') (Map.unionWith (+) w w')


instance Monoid Stats where
  mempty = Stats Map.empty Map.empty


producer :: Handle -> TQueue Event -> IO ()
producer input queue =
  runConduit
     $ sourceHandle input
    .| mapOutputMaybe decodeStrict linesUnboundedAsciiC
    .| mapM_C (atomically . writeTQueue queue)


consumer :: (event -> stats -> stats) -> TQueue event -> TVar stats -> IO ()
consumer f queue var = do
  e <- atomically $ readTQueue queue
  _ <- atomically $ modifyTVar' var (f e)
  consumer f queue var


eventToStats :: Event -> Stats
eventToStats Event{..} = Stats{..}
  where
    statsEvents = Map.singleton eventType 1
    statsWords = Map.unionsWith (+) $ (`Map.singleton` 1) <$> allWords
    allWords = coerce $ Text.words eventData :: [Word]


applyEvent :: Event -> Stats -> Stats
applyEvent event stats = eventToStats event <> stats


type ChallengeAPI
    =  "events" :>
          (  "countByEventType" :> Get '[JSON] EventCount
        :<|> "countWords" :> Get '[JSON] WordCount
          )


challengeAPI :: Proxy ChallengeAPI
challengeAPI = Proxy


server :: TVar Stats -> Server ChallengeAPI
server stats
    =  statsEvents <$> getStats
  :<|> statsWords  <$> getStats
  where
    getStats = liftIO $ atomically $ readTVar stats


main :: IO ()
main = do
  stats <- newTVarIO mempty
  queue <- newTQueueIO

  _ <- forkIO $ producer stdin queue
  _ <- forkIO $ consumer applyEvent queue stats

  let port = 8000
  let
    settings = defaultSettings
      & setPort port
      & setBeforeMainLoop (putStrLn $ "Running http://localhost:" ++ show port)

  runSettings settings $ serve challengeAPI (server stats)
