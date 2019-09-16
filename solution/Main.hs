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


decodeC :: (FromJSON o, Monad m) => ConduitT ByteString o m ()
decodeC = mapOutputMaybe decodeStrict linesUnboundedAsciiC


processor :: TVar Stats -> IO ()
processor stats = 
  runConduit $
    stdinC
    .| decodeC
    .| mapM_C (\e -> atomically $ modifyTVar' stats (applyEvent e))


server :: TVar Stats -> Server ChallengeAPI
server stats
    =  (statsEvents <$> getStats)
  :<|> (statsWords  <$> getStats)
  where
    getStats = liftIO $ readTVarIO stats


main :: IO ()
main = do
  stats <- newTVarIO mempty

  _ <- forkIO $ processor stats

  let
    port = 8000
    settings = defaultSettings
      & setPort port
      & setBeforeMainLoop (putStrLn $ "Running http://localhost:" ++ show port)

  runSettings settings $ serve challengeAPI $ server stats
