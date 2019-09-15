{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


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
import Data.Aeson (decodeStrict)
import Data.Function ((&))
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setPort
  )
import Servant ((:<|>)((:<|>)), (:>), Get, JSON, Proxy(Proxy), Server, serve)
import System.IO (Handle, stdin)

import Model
  ( Event
  , EventCount
  , Stats(statsEvents, statsWords)
  , WordCount
  , applyEvent
  )


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

