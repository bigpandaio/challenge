{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}


module Model
  ( Event
  , EventType
  , UniqueWord
  , Histogram
  , Stats(statsEvents, statsWords)
  , applyEvent
  ) where


import Data.Aeson (FromJSON(parseJSON), ToJSON, ToJSONKey, (.:), withObject)
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


newtype Histogram a =
  Histogram (Map a Int)
  deriving newtype (ToJSON)


instance Ord a => Semigroup (Histogram a) where
  Histogram m <> Histogram m' = Histogram $ Map.unionWith (+) m m'


instance Ord a => Monoid (Histogram a) where
  mempty = Histogram mempty


singletonHistogram :: a -> Histogram a
singletonHistogram = Histogram . (`Map.singleton` 1)


data Stats =
  Stats
    { statsEvents :: !(Histogram EventType)
    , statsWords  :: !(Histogram UniqueWord)
    }


instance Semigroup Stats where
  Stats e w <> Stats e' w' =
    Stats (e <> e') (w <> w')


instance Monoid Stats where
  mempty = Stats mempty mempty


eventToStats :: Event -> Stats
eventToStats Event{..} = Stats{..}
  where
    statsEvents = singletonHistogram eventType
    statsWords = foldMap singletonHistogram allWords
    allWords = coerce @_ @[UniqueWord] $ Text.words eventData


applyEvent :: Event -> Stats -> Stats
applyEvent event stats = eventToStats event <> stats
