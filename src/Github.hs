{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}

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
type RepoName = Text

data User =
  GitHubUser { login :: Text
             , name  :: Maybe Text
             , email :: Maybe Text
             } deriving (Generic, FromJSON, Show)

data Repo =
  Repo { name :: Text
             , language :: Text
             } deriving (Generic, FromJSON, Show)

data Commit = 
  Commit { sha :: Text
          --, author :: Object
              } deriving (Generic, FromJSON, Show)

type GitHubAPI = "users" :> Header "user-agent" UA 
                         :> BasicAuth "github" Int
                         :> Capture "username" UN  :> Get '[JSON] User

            :<|> "users" :> Header "user-agent" UA 
                         :> BasicAuth "github" Int
                         :> Capture "username" UN  :> "repos" :>  Get '[JSON] [Repo]

            :<|> "repos" :> Header  "user-agent" UA
                         :> BasicAuth "github" Int 
                         :> Capture "username" UN  :> Capture "repo"     RepoName  :> "commits" :>  Get '[JSON] [Commit]

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

first :: Maybe UA -> BasicAuthData -> UN -> ClientM User
getRepos :: Maybe UA -> BasicAuthData -> UN -> ClientM [Repo]
getCommits :: Maybe UA -> BasicAuthData -> UN -> RepoName -> ClientM [Commit]

first :<|> getRepos :<|> getCommits = client gitHubAPI
