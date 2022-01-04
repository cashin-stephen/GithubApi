--Stephen Cashin
--cashins

--importing Language mdodules
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( someFunc
    ) where

--Importing all necessary libraries
--Most important is Servant which  will handle the calling of JSON
import qualified Github as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Environment           (getArgs)
import           Data.Text

--highest level Function to invoke all functionality with some explanatory IO
--takes a name parameter to select a user
someFunc :: IO ()
someFunc = do
    putStrLn "about to call"
    (name:_) <- getArgs
    githubCall $ pack name
    putStrLn "end."

-- Function for handling all calls to the API, returns an IO
githubCall :: Text -> IO ()
githubCall name = 
--Servant runs the "first" call on user with the User Agent Haskell App
    (SC.runClientM (GH.first (Just "haskell-app") name) =<< env) >>= \case

--Monad so it has a fail and retun case
        Left err -> do
            putStrLn $ "error has occured: " ++ show err
        Right res -> do
            putStrLn $ "Success! " ++ show res

--Establishing the environemnt as Servant invoking the API in the IO space
    where env :: IO SC.ClientEnv
          env = do
            manager <- newManager tlsManagerSettings
            return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")