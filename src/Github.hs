-- Stephen Cashin
-- cashins

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Github where 


--Importing all necessary libraries
--Most important is Servant which  will handle the calling of JSON
import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

--Establishing custom types for called values
type UN = Text
type UA = Text

--Dataype in which to store the called value, requires a derivation of FromJSON
--to convert JSON into text
data User =
    User {login :: Text}
        deriving (Generic, FromJSON, Show)


--Establishin GitHubAPit which is used to handle all the call to github
type GitHubAPI = "users" :> Header "user-agent" UA
                         :> Capture "username" UN :> Get '[JSON] User
            :<|> "second" :> Get '[JSON] Text


gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

--Establishing typing for the API calls to be made
first :: Maybe UA -> UN -> ClientM User
second :: ClientM Text

first :<|> second = client gitHubAPI