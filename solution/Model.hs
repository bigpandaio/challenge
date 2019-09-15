{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}


module Model
  ( Event
  , EventType
  , UniqueWord
  , Stats(statsEvents, statsWords)
  , WordCount
  , EventCount
  , applyEvent
  ) where


import Data.Aeson (FromJSON(parseJSON), ToJSONKey, (.:), withObject)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text


data Event =
  Event
    { eventType      :: !EventType
    , eventData      :: !Text
    , eventTimestamp :: !Int
    }


instance FromJSON Event where
  parseJSON =
    withObject "Event" $ \v -> do
      eventType      <- v .: "event_type"
      eventData      <- v .: "data"
      eventTimestamp <- v .: "timestamp"
      pure Event {..}


newtype EventType =
  EventType Text
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSONKey)


newtype UniqueWord =
  UniqueWord Text
  deriving newtype (Eq, Ord, ToJSONKey)


data Stats =
  Stats
    { statsEvents :: !EventCount
    , statsWords  :: !WordCount
    }


type EventCount = Map EventType  Int
type WordCount  = Map UniqueWord Int


instance Semigroup Stats where
  Stats e w <> Stats e' w' =
    Stats (Map.unionWith (+) e e') (Map.unionWith (+) w w')


instance Monoid Stats where
  mempty = Stats Map.empty Map.empty


eventToStats :: Event -> Stats
eventToStats Event{..} = Stats{..}
  where
    statsEvents = Map.singleton eventType 1
    statsWords = Map.unionsWith (+) $ (`Map.singleton` 1) <$> allWords
    allWords = coerce $ Text.words eventData :: [UniqueWord]


applyEvent :: Event -> Stats -> Stats
applyEvent event stats = eventToStats event <> stats
