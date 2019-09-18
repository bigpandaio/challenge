{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}


module Model
  ( Histogram
  , Event
  , EventType
  , UniqueWord
  , Stats(statsEvents, statsWords)
  , applyEvent
  ) where


import Data.Aeson (FromJSON(parseJSON), ToJSON, ToJSONKey, (.:), withObject)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text


-- | The core data structure of our state.
--
-- Holds values that can be compared for equality along with a count of their
-- occurrences.
newtype Histogram a =
  Histogram (Map a Int)
  deriving newtype (ToJSON)


-- | Create a histogram with a single occurrence of a value.
singletonHistogram :: a -> Histogram a
singletonHistogram a = Histogram $ Map.singleton a 1


instance Ord a => Semigroup (Histogram a) where
  Histogram m <> Histogram m' =
    -- Appending two histograms is a simple map union, summing the occurrences.
    Histogram $ Map.unionWith (+) m m'


instance Ord a => Monoid (Histogram a) where
  mempty = Histogram mempty


-- | Type representation of the event we're going to process.
data Event =
  Event
    { eventType      :: !EventType
    , eventData      :: !Text -- Not 'UniqueWord', may contain multiple words
    , eventTimestamp :: !Int
    }


instance FromJSON Event where
  parseJSON =
    withObject "Event" $ \v -> do
      eventType      <- v .: "event_type"
      eventData      <- v .: "data"
      eventTimestamp <- v .: "timestamp"
      pure Event {..}


-- | Newtype wrapper for unique observed events.
newtype EventType =
  EventType Text
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSONKey)


-- | Newtype wrapper for unique observed words.
newtype UniqueWord =
  UniqueWord Text
  deriving newtype (Eq, Ord, ToJSONKey)


-- | The main application state.
--
-- Histograms of the data we're accumulating.
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
    -- Record the event type
    statsEvents = singletonHistogram eventType

    -- Record each occurrence of word in 'eventData'
    statsWords = foldMap singletonHistogram allWords
    allWords = coerce @_ @[UniqueWord] $ Text.words eventData


applyEvent :: Event -> Stats -> Stats
applyEvent event stats = eventToStats event <> stats
