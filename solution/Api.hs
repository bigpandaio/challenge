{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Api
  ( ChallengeAPI
  , challengeAPI
  ) where


import Servant ((:<|>), (:>), Get, JSON, Proxy(Proxy))

import Model (EventType, Histogram, UniqueWord)


type ChallengeAPI
    =  "events" :>
          (  "countByEventType" :> Get '[JSON] (Histogram EventType)
        :<|> "countWords"       :> Get '[JSON] (Histogram UniqueWord)
          )


challengeAPI :: Proxy ChallengeAPI
challengeAPI = Proxy
