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
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
import System.IO

--highest level Function to invoke all functionality with some explanatory IO
--takes a name parameter to select a user
someFunc :: IO ()
someFunc = do
    writeFile "cPie.csv" ""
    writeFile "cBar.csv" ""
    writeFile "typeCount.csv" ""
    putStrLn "about to call"
    (uName:user:token:_) <- getArgs
    let auth = BasicAuthData (fromString user) (fromString token)
    print uName
    if ".txt" `isInfixOf` pack uName
        then do
            line <- readLines uName
            print line
            githubCall auth line
            formatTypeCount "cPie.csv" "typeCount.csv"

    else
        do
            print "not file"
            githubCall auth uName
            formatTypeCount "cPie.csv" "typeCount.csv"

readLines :: FilePath -> IO [String]
readLines = fmap Prelude.lines . readFile

class GithubApi a where
    githubCall :: BasicAuthData -> a -> IO ()

instance GithubApi [String] where
    githubCall auth [] = putStrLn "end"
    githubCall auth (x:xs) = do
                                githubCall auth x
                                githubCall auth xs

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
                        let rNames = Prelude.unwords (map (\(GH.Repo n _) -> unpack n) repos)
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
interleave list (x:xs) (y:ys) = list++[(x,y)]++interleave list xs ys

getLang ::GH.Repo -> Maybe Text
getLang (GH.Repo _ Nothing) = Nothing
getLang (GH.Repo _ l) = l

getUserCommits :: BasicAuthData -> IO SC.ClientEnv -> Text -> [(String,Maybe Text,String,String)] -> [(String,Maybe Text)] -> IO ()
getUserCommits _ _ name acc [] = do
                                                let userCommits = removeNonUserCommits name [] acc
                                                exportUserCommits userCommits
                                                let sorteduserCommits = sortBy (\(_,_,_,a) (_,_,_,b) -> compare a b) userCommits
                                                let earlyUserCommits = shortenedUC sorteduserCommits
                                                let pType = determinePType 0 0 0 earlyUserCommits
                                                appendFile "cPie.csv" (pType++"\n")
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
showCommit (GH.Commit sha commitA) = showCommitA commitA

showCommitA :: GH.CommitA -> String -> Maybe Text -> (String,Maybe Text,String,String)
showCommitA (GH.CommitA author) = showAuthor author

showAuthor :: GH.Author -> String -> Maybe Text -> (String,Maybe Text,String,String)
showAuthor (GH.Author name email date) repo lang = (repo, lang, unpack name, unpack date)

removeNonUserCommits :: Text -> [(String,Maybe Text,String,String)] -> [(String,Maybe Text,String,String)] -> [(String,Maybe Text,String,String)]
removeNonUserCommits _ acc [] = acc
removeNonUserCommits name acc ((repo, lang, uName, date):xs) =
                                                                if unpack name == uName
                                                                    then let newAcc = acc++[(repo,lang, uName,date)] in removeNonUserCommits name newAcc xs
                                                                else
                                                                    removeNonUserCommits name acc xs

exportUserCommits :: [(String,Maybe Text,String,String)] -> IO ()
exportUserCommits [] = putStrLn ""
exportUserCommits ((repo,Nothing,uName,date):xs) = exportUserCommits xs
exportUserCommits ((repo,Just lang,uName,date):xs) = do
                                                    appendFile "cBar.csv" (uName ++ ","++ unpack lang ++"\n")
                                                    exportUserCommits xs

determinePType :: Int -> Int -> Int -> [(String,Maybe Text,String,String)] -> String
determinePType func oop other []
  | func > quot (oop+func+other) 2 = "functional"
  | oop > quot (oop+func+other) 2 = "oop"
  | otherwise = "other"
determinePType func oop other ((repo, lang, uName, date):xs)
  | isFunc lang = determinePType (func+1) oop other xs
  | isOop lang = determinePType func (oop+1) other xs
  | otherwise = determinePType func oop (other+1) xs

shortenedUC :: [(String,Maybe Text,String,String)] -> [(String,Maybe Text,String,String)]
shortenedUC list = Prelude.take (quot (Prelude.length list) 4) list

isFunc :: Maybe Text -> Bool
isFunc Nothing = False
isFunc (Just lang) = do
                let funcLang = ["Haskell","Clean","Scala","Erlang","Clojure","SML","F#","Scheme","XSLT","SQL","Mathematica","Elixir","Elm","PureScript","Racket","Reason","Swift","Nix","Emacs","Lua","TSQL","Emacs Lisp"]
                unpack lang `elem` funcLang

isOop :: Maybe Text -> Bool
isOop Nothing = False
isOop (Just lang) = do
    let oopLang = ["Python","C","C++","Java","JavaScript","Go","Ruby","C#","R","PHP","Visual Basic .Net","Perl","Dart","Kotlin","CommonLisp","MATLAB","Samlltalk","Groovy","CoffeeScript","Powershell", "Objective-C"]
    unpack lang `elem` oopLang

formatTypeCount :: FilePath -> FilePath -> IO ()
formatTypeCount input output = do
                                i <- openFile input ReadMode
                                contents <- hGetContents i
                                let lData = Prelude.lines contents
                                let oopC = 0
                                let funcC = 0
                                let otherC = 0
                                let typeCount = countTypes oopC funcC otherC lData
                                appendFile output ("type,n" ++ "\n" ++
                                    "oop," ++ show (first typeCount) ++ "\n" ++
                                    "functional," ++ show (second typeCount) ++ "\n" ++
                                    "other," ++ show (third typeCount))

countTypes :: Int -> Int -> Int -> [String] -> (Int,Int,Int)
countTypes oopC funcC otherC [] = (oopC,funcC,otherC)
countTypes oopC funcC otherC (x:xs)
    | x == "oop" = countTypes (oopC+1) funcC otherC xs
    | x == "functional" = countTypes oopC (funcC+1) otherC xs
    | x == "other" = countTypes oopC funcC (otherC+1) xs
    
first :: (a,a,a) -> a
first (x,_,_) = x

second :: (a,a,a) -> a
second (_,x,_) = x

third :: (a,a,a) -> a
third (_,_,x) = x