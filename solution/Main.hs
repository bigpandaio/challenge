module Main where


import Conduit
  ( ConduitT
  , (.|)
  , linesUnboundedAsciiC
  , mapM_C
  , mapOutputMaybe
  , runConduit
  , stdinC
  )
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  )
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, decodeStrict)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setPort
  )
import Servant ((:<|>)((:<|>)), Server, serve)

import Api (ChallengeAPI, challengeAPI)
import Model (Stats(statsEvents, statsWords), applyEvent)


-- | Conduit pipe taking 'ByteString's, yielding parsed values from each line.
--
-- The combination of 'linesUnboundedAsciiC' and 'mapOutputMaybe' makes it so
-- garbage lines are simply discarded.
decodeC :: (FromJSON o, Monad m) => ConduitT ByteString o m ()
decodeC = mapOutputMaybe decodeStrict linesUnboundedAsciiC


-- What remains now are our two major components.


-- | Our stream processor.
--
-- This takes a 'TVar' 'Stats', reads stdin (buffered at ~64k bytes) and
-- updates the state for successfully parsed event.
processor :: TVar Stats -> IO ()
processor stats = 
  runConduit $
    stdinC
    .| decodeC
    .| mapM_C (\e -> atomically $ modifyTVar' stats (applyEvent e))


-- | Our API server.
--
-- This takes a 'TVar' 'Stats', and for each incoming request efficiently reads
-- the state as witnessed at the point evaluation.
server :: TVar Stats -> Server ChallengeAPI
server stats
    =  (statsEvents <$> getStats)
  :<|> (statsWords  <$> getStats)
  where
    getStats = liftIO $ readTVarIO stats


-- The way these two major components are implemented allow for a good deal of
-- flexibility in future refactoring.
--
-- For example, we could easily modify the code to "shard" the processor, or
-- alter it to batch events at certain intervals to commit larger, fewer
-- events.


main :: IO ()
main = do

  -- Allocate state var
  stats <- newTVarIO mempty

  -- Fork off a thread to process events
  _ <- forkIO $ processor stats

  -- Serve our API via HTTP
  let
    port = 8000
    settings = defaultSettings
      & setPort port
      & setBeforeMainLoop (putStrLn $ "Running http://localhost:" ++ show port)

  runSettings settings $ serve challengeAPI $ server stats
