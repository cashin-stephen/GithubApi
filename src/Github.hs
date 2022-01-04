-- Stephen Cashin
-- cashins

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Github where 

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

type UN = Text
type UA = Text

type GitHubAPi = "users" :> Header "user-agent" UA
                         :> Capture "username" UN :> Get '[JSON] Text
            :<|> "second" :> Get '[JSON] Text

gitHubAPI :: Proxy GitHubAPi
gitHubAPI = Proxy

first :: Maybe UA -> UN -> ClientM Text
second :: ClientM Text

first :<|> second = client gitHubAPI