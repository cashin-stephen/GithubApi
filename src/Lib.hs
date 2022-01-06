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
    githubCall auth $ uName
    putStrLn "end."

class GithubApi a where
    githubCall :: BasicAuthData -> a -> IO ()

instance GithubApi [String] where    
    githubCall auth [] = putStrLn $ "end"
    githubCall auth (x:xs) = do
                                githubCall auth x

-- Function for handling all calls to the API, returns an IO
instance GithubApi String where 
    githubCall auth name = 
    --Servant runs the "first" call on user with the User Agent Haskell App
        (SC.runClientM (GH.first (Just "haskell-app") auth (pack name)) =<< env) >>= \case 
        

    --Monad so it has a fail and retun case
            Left err -> do
                putStrLn $ "error has occured: " ++ show err
            Right res -> do
                putStrLn $ "Success! " ++ show res
            
                --get user repos
                (SC.runClientM (GH.getRepos (Just "haskell-app") auth (pack name) ) =<< env) >>= \case
                    Left err -> do
                        putStrLn $ "Problem getting repos: " ++ show err
                    Right repos -> do
                        let rNames =  Prelude.unwords (map (\(GH.Repo n _) -> unpack n) repos)
                        let listrNames = Prelude.words rNames
                        let rLanguages = map getLang repos
                        let languageKey = interleave [] listrNames rLanguages

                        --get User Commits for each of their Repos
                        getUserCommits auth env (pack name) [] languageKey 
                        
        --Establishing the environemnt as Servant invoking the API in the IO space                
        where env = do
                manager <- newManager tlsManagerSettings
                return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")                            

    
        
    
interleave :: [(a,b)] -> [a] -> [b] -> [(a,b)]
interleave list _ [] =  list
interleave list [] _ = list
interleave list (x:xs) (y:ys) = list++[(x,y)]++(interleave list xs ys)

getLang ::GH.Repo -> Maybe Text
getLang (GH.Repo _ Nothing) = Nothing
getLang (GH.Repo _ l) = l

getUserCommits :: BasicAuthData -> IO SC.ClientEnv -> Text -> [(String,Maybe Text,String,String)] -> [(String,Maybe Text)] -> IO ()
getUserCommits _ _ name acc [] = do 
                                                let userCommits = removeNonUserCommits name [] acc
                                                let sorteduserCommits = sortBy (\(_,_,_,a) (_,_,_,b) -> compare a b) userCommits
                                                let earlyUserCommits = shortenedUC sorteduserCommits
                                                let pType = determinePType 0 0 0 earlyUserCommits
                                                print earlyUserCommits
                                                print pType
                                    
getUserCommits auth env name acc ((repo,lang):xs) = (SC.runClientM (GH.getCommits (Just "haskell-app") auth name (pack repo)) =<< env) >>= \case
                                                Left err -> do
                                                    putStrLn $ "Problem getting commits: " ++ show err
                                                Right commits -> do
                                                    let authorList =  map (\xx -> showCommit xx repo lang) commits
                                                    let newAcc = acc++authorList
                                                    getUserCommits auth env name newAcc xs

                                
showCommit ::  GH.Commit -> String -> Maybe Text -> (String,Maybe Text,String,String)
showCommit (GH.Commit sha commitA) repo lang = showCommitA commitA repo lang

showCommitA :: GH.CommitA -> String -> Maybe Text -> (String,Maybe Text,String,String)
showCommitA (GH.CommitA author) repo lang = showAuthor author repo lang

showAuthor :: GH.Author -> String -> Maybe Text -> (String,Maybe Text,String,String)
showAuthor (GH.Author name email date) repo lang = (repo, lang, unpack name, unpack date)

removeNonUserCommits :: Text -> [(String,Maybe Text,String,String)] -> [(String,Maybe Text,String,String)] -> [(String,Maybe Text,String,String)]
removeNonUserCommits _ acc [] = acc
removeNonUserCommits name acc ((repo, lang, uName, date):xs) = do
                                                                if (unpack name) == uName
                                                                    then let newAcc = acc++[(repo,lang, uName,date)] in removeNonUserCommits name newAcc xs
                                                                else
                                                                    removeNonUserCommits name acc xs

determinePType :: Int -> Int -> Int -> [(String,Maybe Text,String,String)] -> String
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

shortenedUC :: [(String,Maybe Text,String,String)] -> [(String,Maybe Text,String,String)]
shortenedUC list = Prelude.take (quot (Prelude.length list) 4) list

isFunc :: Maybe Text -> Bool
isFunc Nothing = False
isFunc (Just lang) = do
                let funcLang = ["Haskell","Clean","Scala","Erlang","Clojure","SML","F#","Scheme","XSLT","SQL","Mathematica","Elixir","Elm","PureScript","Racket","Reason","Swift","Nix","Emacs","Lua","TSQL"]
                if (elem (unpack lang) funcLang)
                    then True
                else
                    False

isOop :: Maybe Text -> Bool
isOop Nothing = False
isOop (Just lang) = do
    let oopLang = ["Python","C","C++","Java","JavaScript","Go","Ruby","C#","R","PHP","Visual Basic .Net","Perl","Dart","Kotlin","CommonLisp","MATLAB","Samlltalk","Groovy","CoffeeScript","Powershell", "Objective-C"]
    if (elem (unpack lang) oopLang)
        then True
    else
        False