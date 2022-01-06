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
import Data.List (intercalate, groupBy, sortBy, sort)
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
                    let languageKey = interleave listrNames listrLanguages
                    print languageKey

                    --get User Commits for each of their Repos
                    getUserCommits auth env name [] languageKey 
                        

--Establishing the environemnt as Servant invoking the API in the IO space
    where env :: IO SC.ClientEnv
          env = do
            manager <- newManager tlsManagerSettings
            return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")
    
interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (Prelude.zipWith (\x y -> [x]++[y]) xs ys)

getUserCommits :: BasicAuthData -> IO SC.ClientEnv -> Text -> [(String,String,String,String)] -> [String] -> IO ()
getUserCommits _ _ name acc [] = do 
                                                let userCommits = removeNonUserCommits name [] acc
                                                let sorteduserCommits = sortBy (\(_,_,_,a) (_,_,_,b) -> compare a b) userCommits
                                                let earlyUserCommits = shortenedUC sorteduserCommits
                                                let pType = determinePType 0 0 0 earlyUserCommits
                                                print earlyUserCommits
                                                print pType
                                    
getUserCommits auth env name acc (repo:lang:xs) = (SC.runClientM (GH.getCommits (Just "haskell-app") auth name (pack repo)) =<< env) >>= \case
                                                Left err -> do
                                                    putStrLn $ "Problem getting commits: " ++ show err
                                                Right commits -> do
                                                    let authorList =  map (\xx -> showCommit xx repo lang) commits
                                                    let newAcc = acc++authorList
                                                    getUserCommits auth env name newAcc xs

                                
showCommit ::  GH.Commit -> String -> String -> (String,String,String,String)
showCommit (GH.Commit sha commitA) repo lang = showCommitA commitA repo lang

showCommitA :: GH.CommitA -> String -> String -> (String,String,String,String)
showCommitA (GH.CommitA author) repo lang = showAuthor author repo lang

showAuthor :: GH.Author -> String -> String -> (String,String,String,String)
showAuthor (GH.Author name email date) repo lang = (repo, lang, unpack name, unpack date)

removeNonUserCommits :: Text -> [(String,String,String,String)] -> [(String,String,String,String)] -> [(String,String,String,String)]
removeNonUserCommits _ acc [] = acc
removeNonUserCommits name acc ((repo, lang, uName, date):xs) = do
                                                                if (unpack name) == uName
                                                                    then let newAcc = acc++[(repo,lang, uName,date)] in removeNonUserCommits name newAcc xs
                                                                else
                                                                    removeNonUserCommits name acc xs

determinePType :: Int -> Int -> Int -> [(String,String,String,String)] -> String
determinePType func oop other [] =                              if (func > (quot (oop+func+other) 2))
                                                                    then "User is a functional Programmer"
                                                                else if (oop > (quot (oop+func+other) 2))
                                                                    then "User is an oop Programmer"
                                                                else
                                                                    "User is neither a functional nor oop Programmer"
determinePType func oop other ((repo, lang, uName, date):xs) =  if(isFunc lang)
                                                                    then determinePType (func+1) oop other xs
                                                                else if (isOop lang)
                                                                    then determinePType func (oop+1) other xs
                                                                else
                                                                    determinePType func oop (other+1) xs

shortenedUC :: [(String,String,String,String)] -> [(String,String,String,String)]
shortenedUC list = Prelude.take (quot (Prelude.length list) 4) list

isFunc :: String -> Bool
isFunc lang = do
                let funcLang = ["Haskell","Clean","Scala","Erlang","Clojure","SML","F#","Scheme","XSLT","SQL","Mathematica","Elixir","Elm","PureScript","Racket","Reason","Swift","Nix","Emacs","Lua","TSQL"]
                if (elem lang funcLang)
                    then True
                else
                    False

isOop :: String -> Bool
isOop lang = do
    let oopLang = ["Python","C","C++","Java","JavaScript","Go","Ruby","C#","R","PHP","Visual Basic .Net","Perl","Dart","Kotlin","CommonLisp","MATLAB","Samlltalk","Groovy","CoffeeScript","Powershell", "Objective-C"]
    if (elem lang oopLang)
        then True
    else
        False