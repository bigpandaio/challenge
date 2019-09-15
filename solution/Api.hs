{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Api
  ( ChallengeAPI
  , challengeAPI
  ) where


import Servant ((:<|>), (:>), Get, JSON, Proxy(Proxy))

import Model (EventCount, WordCount)


type ChallengeAPI
    =  "events" :>
          (  "countByEventType" :> Get '[JSON] EventCount
        :<|> "countWords"       :> Get '[JSON] WordCount
          )


challengeAPI :: Proxy ChallengeAPI
challengeAPI = Proxy
