--ONLY FOR ARCHIVAL PURPOSES
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
import Data.Text hiding (map,intercalate, groupBy, concat)
import Data.List (intercalate, groupBy, sortBy)
import Data.Either
import           Servant.API                (BasicAuthData (..))
import Data.ByteString.UTF8 (fromString)
import Data.Typeable

--highest level Function to invoke all functionality with some explanatory IO
--takes a name parameter to select a user
someFunc :: IO ()
someFunc = do
    putStrLn "about to call"
    (uName:user:token:_) <- getArgs
    let auth = BasicAuthData (fromString user) (fromString token)
    githubCall auth $ pack uName
    putStrLn "end."

-- Function for handling all calls to the API, returns an IO
githubCall :: BasicAuthData -> Text -> IO ()
githubCall auth name = 
--Servant runs the "first" call on user with the User Agent Haskell App
    (SC.runClientM (GH.first (Just "haskell-app") auth name) =<< env) >>= \case

--Monad so it has a fail and retun case
        Left err -> do
            putStrLn $ "error has occured: " ++ show err
        Right res -> do
            putStrLn $ "Success! " ++ show res
        
             --get user repos
            (SC.runClientM (GH.getRepos (Just "haskell-app") auth name ) =<< env) >>= \case
                Left err -> do
                    putStrLn $ "Problem getting repos: " ++ show err
                Right repos -> do
                    let rNames =  Prelude.unwords (map (\(GH.Repo n _) -> unpack n) repos)
                    let rLanguages = Prelude.unwords (map (\(GH.Repo _ l) -> unpack l) repos)
                    let listrNames = Prelude.words rNames
                    let listrLanguages = Prelude.words rLanguages
                    let repoList = interleave listrNames listrLanguages
                    print repoList

                    --get User Commits for each of their Repos
                    getUserCommits auth env name listrNames
                        

--Establishing the environemnt as Servant invoking the API in the IO space
    where env :: IO SC.ClientEnv
          env = do
            manager <- newManager tlsManagerSettings
            return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")
    
interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (Prelude.zipWith (\x y -> [x]++[y]) xs ys)

getUserCommits :: BasicAuthData -> IO SC.ClientEnv -> Text -> [String] -> IO ()
getUserCommits _ _ _ [] = putStrLn "End of commits"
getUserCommits auth env name (x:xs) = (SC.runClientM (GH.getCommits (Just "haskell-app") auth name (pack x)) =<< env) >>= \case
                                Left err -> do
                                    putStrLn $ "Problem getting commits: " ++ show err
                                Right commits -> do
                                    --putStrLn $ "Commits are: " ++ intercalate ", " (map (\(GH.Commit n _) -> unpack n) commits) ++
                                    let authorList =  ((map showCommit) commits)
                                    let repoAuthorList = concatr x authorList
                                    print repoAuthorList
                                    --putStrLn $  "Query is : " ++ (unpack name) ++ " " ++ x ++ "\n" ++
                                    --    "Commits are: " ++ intercalate ", " (map (\(GH.Commit _ n) -> unpack n) commits)
                                    getUserCommits auth env name xs
                                
showCommit ::  GH.Commit -> [String]
showCommit (GH.Commit sha commitA) = showCommitA commitA

showCommitA :: GH.CommitA -> [String]
showCommitA (GH.CommitA author) = showAuthor author

showAuthor :: GH.Author -> [String]
showAuthor (GH.Author name email date) = [unpack name, unpack date]

concatr :: String -> [[String]] -> [[String]]
concatr x ys = map (x:) ys