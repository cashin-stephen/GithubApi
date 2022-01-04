--Stephen Cashin
--cashins

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

import qualified Github as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)

someFunc :: IO ()
someFunc = do
    putStrLn "about to call"
    githubCall
    putStrLn "end."

githubCall :: IO ()
githubCall = 
    (SC.runClientM (GH.first (Just "haskell-app") "cashin-stephen") =<< env) >>= \case

        Left err -> do
            putStrLn $ "error has occured: " ++ show err
        Right res -> do
            putStrLn $ "Success! " ++ show res
    
    where env :: IO SC.ClientEnv
          env = do
            manager <- newManager tlsManagerSettings
            return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")