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
import Data.Text as DT hiding (map,intercalate, groupBy, concat)
import Data.List (intercalate, groupBy, sortBy, sort, nub)
import Data.List.Split as DS
import Data.Either
import           Servant.API                (BasicAuthData (..))
import Data.ByteString.UTF8 (fromString)
import Data.Typeable
import System.IO

--highest level Function to invoke all functionality with some explanatory IO
--effectively acts as the main doign all functionality from sourcing, parsing and exporting the data
--takes a name parameter to select a user

someFunc :: IO ()
someFunc = do
    writeFile "vis/vis-yesod/data/cPie.csv" ""
    writeFile "vis/vis-yesod/data/cBar.csv" ""
    writeFile "vis/vis-yesod/data/typeCount.csv" ""
    writeFile "vis/vis-yesod/data/totalLang.csv" ""
    writeFile "vis/vis-yesod/data/totalfLang.csv" ""
    writeFile "vis/vis-yesod/data/yearLang.csv" ""
    writeFile "vis/vis-yesod/data/yearfLang.csv" ""
    putStrLn "about to call"
    (uName:user:token:_) <- getArgs
    let auth = BasicAuthData (fromString user) (fromString token)
    print uName
    if ".txt" `isInfixOf` pack uName
        then do
            line <- readLines uName
            print line
            githubCall auth line
            formatTypeCount "vis/vis-yesod/data/cPie.csv" "vis/vis-yesod/data/typeCount.csv"
            formatLang "vis/vis-yesod/data/cBar.csv" "vis/vis-yesod/data/totalLang.csv" "vis/vis-yesod/data/totalfLang.csv" "vis/vis-yesod/data/yearLang.csv" "vis/vis-yesod/data/yearfLang.csv"

    else
        do
            print "not file"
            githubCall auth uName
            formatTypeCount "vis/vis-yesod/data/cPie.csv" "vis/vis-yesod/data/typeCount.csv"
            formatLang "vis/vis-yesod/data/cBar.csv" "vis/vis-yesod/data/totalLang.csv" "vis/vis-yesod/data/totalfLang.csv" "vis/vis-yesod/data/yearLang.csv" "vis/vis-yesod/data/yearfLang.csv"

-- Read a file line by line returning the content as a list of Strings
-- takes a filePath as a parameter
readLines :: FilePath -> IO [String]
readLines = fmap Prelude.lines . readFile

-- ad-hoc polymorphic declaration of the Github Api call. this can either be a list of usernames or just a username
class GithubApi a where
    githubCall :: BasicAuthData -> a -> IO ()

-- version of GithubApi that takes a list of usernames
-- recursively works through the list calling the singualr version of GithubApi on eac
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
                let user = getUser res

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
                        getUserCommits auth env user (pack name) [] languageKey


        --Establishing the environemnt as Servant invoking the API in the IO space                
        where env = do
                manager <- newManager tlsManagerSettings
                return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")

-- Simple get function to isolate username from the User data type
-- Takes User
-- returns Maybe version of username as text
getUser :: GH.User -> Maybe Text
getUser (GH.User _ Nothing _) = Nothing
getUser (GH.User _ n _) = n

-- Function to combine each element of 2 lists into a tuple with both elements
-- takes an empty accumulator to store the result in, and the two lists
-- returns the then tuplified list
interleave :: [(a,b)] -> [a] -> [b] -> [(a,b)]
interleave list _ [] =  list
interleave list [] _ = list
interleave list (x:xs) (y:ys) = list++[(x,y)]++interleave list xs ys

-- Simple get function to isolate lang from the User REpo type
-- Takes Repo
-- returns Maybe version of alng as text
getLang :: GH.Repo -> Maybe Text
getLang (GH.Repo _ Nothing) = Nothing
getLang (GH.Repo _ l) = l

-- function to do a lot:
-- - recursively works through the commit data to format, sort and isolate early commits from the user history 
-- - export data to data file containg unformatted data for pie chart to count total user types
-- returns IO commands such as printing the type (functional or object oriented) of the user
getUserCommits :: BasicAuthData -> IO SC.ClientEnv -> Maybe Text -> Text -> [(String,Maybe Text,String,String)] -> [(String,Maybe Text)] -> IO ()
getUserCommits _ _ user name acc [] = do
                                                let userCommits = removeNonUserCommits user name [] acc
                                                --print userCommits
                                                let sorteduserCommits = sortBy (\(_,_,_,a) (_,_,_,b) -> compare a b) userCommits
                                                let earlyUserCommits = shortenedUC sorteduserCommits
                                                let pType = determinePType 0 0 0 earlyUserCommits
                                                exportUserCommits pType userCommits
                                                appendFile "vis/vis-yesod/data/cPie.csv" (pType++"\n")
                                                --print earlyUserCommits
                                                print pType


getUserCommits auth env user name acc ((repo,lang):xs) = (SC.runClientM (GH.getCommits (Just "haskell-app") auth name (pack repo)) =<< env) >>= \case
                                                Left err -> do
                                                    putStrLn $ "Problem getting commits: " ++ show err
                                                Right commits -> do
                                                    let authorList =  map (\xx -> showCommit xx repo lang) commits
                                                    let newAcc = acc++authorList
                                                    getUserCommits auth env user name newAcc xs

--step 1 of working through the commit Data type to ultimately get the Author
showCommit ::  GH.Commit -> String -> Maybe Text -> (String,Maybe Text,String,String)
showCommit (GH.Commit sha commitA) = showCommitA commitA

--step 2 of working through the commit Data type to ultimately get the Author
showCommitA :: GH.CommitA -> String -> Maybe Text -> (String,Maybe Text,String,String)
showCommitA (GH.CommitA author) = showAuthor author

--step 1 of working through the commit Data type to ultimately get the Author
--returns a tuple containing the repository, the language of the repo, the name of the author and the date of the commit
showAuthor :: GH.Author -> String -> Maybe Text -> (String,Maybe Text,String,String)
showAuthor (GH.Author name email date) repo lang = (repo, lang, unpack name, unpack date)

-- function to remove commits from the commit list that were not made by yhe user in question
-- takes login id and name of the user and the list of commit data
-- recursively checks if the username is the same as the one marked on the commit adding it to the accumulator if true
-- returns the accumulator as the final lsit containing the commit data owned by the user
removeNonUserCommits :: Maybe Text -> Text -> [(String,Maybe Text,String,String)] -> [(String,Maybe Text,String,String)] -> [(String,Maybe Text,String,String)]
removeNonUserCommits _ _ acc [] = acc
removeNonUserCommits Nothing name acc ((repo, lang, uName, date):xs) =
                                                                if unpack name == uName
                                                                    then let newAcc = acc++[(repo,lang, uName,date)] in removeNonUserCommits Nothing name newAcc xs
                                                                else
                                                                    removeNonUserCommits Nothing name acc xs
removeNonUserCommits (Just user) name acc ((repo, lang, uName, date):xs) =
                                                                if unpack name == uName || unpack user == uName
                                                                    then let newAcc = acc++[(repo,lang, uName,date)] in removeNonUserCommits (Just user) name newAcc xs
                                                                else
                                                                    removeNonUserCommits (Just user) name acc xs

-- writes the user commit data to a file that will be used for further formatting
-- takes the type of the user and a list of all their commit data
-- return a write command in the IO space
exportUserCommits :: String -> [(String,Maybe Text,String,String)] -> IO ()
exportUserCommits _ [] = appendFile "vis/vis-yesod/data/cBar.csv" ("---" ++"\n")
exportUserCommits pType ((repo,Nothing,uName,date):xs) = exportUserCommits pType xs
exportUserCommits pType ((repo,Just lang,uName,date):xs) = do
                                                    appendFile "vis/vis-yesod/data/cBar.csv" (pType ++ ","++ unpack lang ++ "," ++ Prelude.take 4 date ++"\n" )
                                                    exportUserCommits pType xs

--Function to determine the type of the the user by counting how many of their early commits were in a functional, object oriented language
-- takes 3 accumulators one for each type and the list of their commits
-- returns functional, oop or other 
determinePType :: Int -> Int -> Int -> [(String,Maybe Text,String,String)] -> String
determinePType func oop other []
  | func > quot (oop+func+other) 2 = "functional"
  | oop > quot (oop+func+other) 2 = "oop"
  | otherwise = "other"
determinePType func oop other ((repo, lang, uName, date):xs)
  | isFunc lang = determinePType (func+1) oop other xs
  | isOop lang = determinePType func (oop+1) other xs
  | otherwise = determinePType func oop (other+1) xs


-- Isolates approximately the first 25% of a users commits for determining their type
-- takes a list of user commit data
-- returns a shortened version of list
shortenedUC :: [(String,Maybe Text,String,String)] -> [(String,Maybe Text,String,String)]
shortenedUC list = Prelude.take (quot (Prelude.length list) 4) list

-- a higherlevel function to determine the type of a language
-- Takes a lang
-- returns the type
getLangType ::  Text -> String
getLangType lang
    | isFunc lang = "functional"
    | isOop lang = "oop"
    | otherwise = "other"

-- ad hoc polymorphic declaration of the isFunc function
class IsFunc a where
    isFunc :: a -> Bool

-- implementation of isFunc that takes a maybe version of lang
-- determines whether a lang is functional by checking against a static list
instance IsFunc (Maybe Text) where
    isFunc Nothing = False
    isFunc (Just lang) = do
                    let funcLang = ["Haskell","Clean","Scala","Erlang","Clojure","SML","F#","Scheme","XSLT","SQL","Mathematica","Elixir","Elm","PureScript","Racket","Reason","Swift","Nix","Emacs","Lua","TSQL","Emacs Lisp","R"]
                    unpack lang `elem` funcLang

-- implementation of isFunc that takes a Text version of lang
-- determines whether a lang is functional by checking against a static list
instance IsFunc Text where
    isFunc lang = do
                    let funcLang = ["Haskell","Clean","Scala","Erlang","Clojure","SML","F#","Scheme","XSLT","SQL","Mathematica","Elixir","Elm","PureScript","Racket","Reason","Swift","Nix","Emacs","Lua","TSQL","Emacs Lisp","R"]
                    unpack lang `elem` funcLang

-- ad hoc polymorphic declaration of the isOop function
class IsOop a where
    isOop :: a -> Bool

-- implementation of isOop that takes a maybe version of lang
-- determines whether a lang is object oriented by checking against a static list
instance IsOop (Maybe Text) where
    isOop Nothing = False
    isOop (Just lang) = do
        let oopLang = ["Python","C","C++","Java","TypeScript","Go","Ruby","C#","Visual Basic .Net","Perl","Dart","Kotlin","CommonLisp","MATLAB","Samlltalk","Groovy","Powershell", "Objective-C","ActionScript"]
        unpack lang `elem` oopLang

-- implementation of isOop that takes a Text version of lang
-- determines whether a lang is object oriented by checking against a static list
instance IsOop Text where
    isOop lang = do
        let oopLang = ["Python","C","C++","Java","TypeScript","Go","Ruby","C#","Visual Basic .Net","Perl","Dart","Kotlin","CommonLisp","MATLAB","Samlltalk","Groovy","Powershell", "Objective-C","ActionScript"]
        unpack lang `elem` oopLang

-- Function to foramt and write the file that shows the count for each type in the dataset
-- takes in an input FilePath from which to read data, formats the data and writes to the output file
-- returns the write command in the IO space
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
                                    "oop," ++ show (first3 typeCount) ++ "\n" ++
                                    "functional," ++ show (second3 typeCount) ++ "\n" ++
                                    "other," ++ show (third3 typeCount))

-- takes 3 accumulators for each type and counts the occurences of each type in the list of all type occurences
-- totalling and storing in a tuple
countTypes :: Int -> Int -> Int -> [String] -> (Int,Int,Int)
countTypes oopC funcC otherC [] = (oopC,funcC,otherC)
countTypes oopC funcC otherC (x:xs)
    | x == "oop" = countTypes (oopC+1) funcC otherC xs
    | x == "functional" = countTypes oopC (funcC+1) otherC xs
    | x == "other" = countTypes oopC funcC (otherC+1) xs

-- Function that formats and writes data for the bar charts and line charts
-- takes the filepath for the 2 bar charts and the 2 line charts
-- returns writing in the IO space
formatLang :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO()
formatLang input output foutput youtput fyoutput = do
                            i <- openFile input ReadMode
                            contents <- hGetContents i
                            let lData = DS.splitOn ["---"] (Prelude.lines contents)
                            let tlData = doubleListToText [] lData
                            let tupData = sortAll [] (tuplifyAll [] tlData)
                            let flatData = doubleFlattenList [] tlData
                            let activeYears = listToString [] (sort (nub (getActiveYears [] (concat tupData))))
                            let langCounts = langCountAll (0,0,0,0) (doubleListToString [] flatData) Nothing
                            let flangCounts = fLangCountAll (0,0,0,0) (doubleListToString [] flatData) Nothing
                            let langCountByyear = countYearLangsbyYear [] activeYears (doubleListToString [] flatData) Nothing
                            print langCountByyear
                            let flangCountByyear = countYearfLangsbyYear [] activeYears (doubleListToString [] flatData) Nothing
                            print flangCountByyear
                            appendFile youtput ("year,type,lang" ++ "\n")
                            writeCountYearLangsbyYear youtput langCountByyear
                            appendFile fyoutput ("year,type,flang" ++ "\n")
                            writeCountYearLangsbyYear fyoutput flangCountByyear
                            appendFile output ("type,lang" ++ "\n" ++
                                "oop," ++ show (first4 langCounts/third4 langCounts) ++ "\n" ++
                                "functional," ++ show (second4 langCounts/fourth4 langCounts) ++ "\n")
                            appendFile foutput ("type,lang" ++ "\n" ++
                                "oop," ++ show (first4 flangCounts/third4 flangCounts) ++ "\n" ++
                                "functional," ++ show (second4 flangCounts/fourth4 flangCounts) ++ "\n")

-- secondary function to recursively write the data a file formatted for line chart
-- take file path and commit data
writeCountYearLangsbyYear :: FilePath -> [(String,Float,Float,Float,Float)] -> IO ()
writeCountYearLangsbyYear _ [] = return ()
writeCountYearLangsbyYear output ((year,oopC,funcC,oopT,funcT):xs) = do
    appendFile output (year ++ ",oop," ++ show (oopC/oopT) ++ "\n" ++
                       year ++ ",func," ++ show (funcC/funcT) ++ "\n")
    writeCountYearLangsbyYear output xs

-- Function that takes list of commit data, list of years and a type flag that determines the type of the user.
-- Works recursively through the list adding an entry containg information about unique languages encountered for each year
countYearLangsbyYear :: [(String,Float,Float,Float,Float)] -> [String] -> [[String]] -> Maybe Bool -> [(String,Float,Float,Float,Float)]
countYearLangsbyYear finalList [] _ _ = finalList
countYearLangsbyYear acc (x:xs) tupData typeFlag = acc++[countYearLangAll (x,0,0,0,0) tupData typeFlag]++ countYearLangsbyYear acc xs tupData typeFlag

-- Function that takes a list containing commit info and an accumulator a type flag
-- Works recursively through the info accumualting the relevant counts
countYearLangAll :: (String,Float,Float,Float,Float) -> [[String]] -> Maybe Bool -> (String,Float,Float,Float,Float)
countYearLangAll acc [] _ = acc
countYearLangAll (year,oopC,funcC,oopT,funcT) (x:xs) typeFlag = add4Tuple5 (countYearLang year [] [] oopC funcC oopT funcT x typeFlag) (countYearLangAll (year,oopC,funcC,oopT,funcT) xs typeFlag)

-- Function that counts the number of languages used by a user in each year
-- takes a year string, an accumulated list of oop and func languages to watch for duplicates,
-- also takes an accumulated count of unique languages for both func and oop
-- also takes an accumulated count of total users of oop and func type informed by the typeFlag
-- returns a tuple containing year and the counts
countYearLang :: String -> [String] -> [String] -> Float -> Float -> Float -> Float -> [String] -> Maybe Bool -> (String,Float,Float,Float,Float)
countYearLang year uOop uFunc oopC funcC oopT funcT [] typeFlag
    | typeFlag == Nothing = (year,oopC,funcC,oopT,funcT)
    | typeFlag == Just False = (year,oopC,funcC,oopT,funcT+1)
    | typeFlag == Just True = (year,oopC,funcC,oopT+1,funcT)
    | otherwise = (year,oopC,funcC,oopT,funcT)
countYearLang year uOop uFunc oopC funcC oopT funcT (x:y:z:xs) typeFlag
    | x == "oop" =
         if y `elem` uOop then
                countYearLang year uOop uFunc oopC funcC oopT funcT xs typeFlag
            else
                do
                    let newTypeFlag = Just True
                    let newuOop = uOop ++ [y]
                    if z == year then
                        countYearLang year newuOop uFunc (oopC+1) funcC oopT funcT xs newTypeFlag
                    else
                        countYearLang year uOop uFunc oopC funcC oopT funcT xs newTypeFlag
    | x == "functional" = 
            if y `elem` uFunc then
                countYearLang year uOop uFunc oopC funcC oopT funcT xs typeFlag
            else
                do
                    let newuFunc = uFunc ++ [y]
                    let newTypeFlag = Just False
                    if z == year then
                        countYearLang year uOop newuFunc oopC (funcC+1) oopT funcT xs newTypeFlag
                    else
                        countYearLang year uOop uFunc oopC funcC oopT funcT xs newTypeFlag
    | otherwise =  countYearLang year uOop uFunc oopC funcC oopT funcT xs typeFlag

-- Function that takes list of commit data, list of years and a type flag that determines the type of the user.
-- Works recursively through the list adding an entry containg information about unique foreign languages encountered for each year
countYearfLangsbyYear :: [(String,Float,Float,Float,Float)] -> [String] -> [[String]] -> Maybe Bool -> [(String,Float,Float,Float,Float)]
countYearfLangsbyYear finalList [] _ _ = finalList
countYearfLangsbyYear acc (x:xs) tupData typeFlag = let previousYears = countYearfLangAll (x,0,0,0,0) tupData typeFlag in acc++[previousYears]++ countYearfLangsbyYear acc xs tupData typeFlag

-- Function that takes a list containing commit info and an accumulator a type flag
-- Works recursively through the info accumualting the relevant counts
countYearfLangAll :: (String,Float,Float,Float,Float) -> [[String]] -> Maybe Bool -> (String,Float,Float,Float,Float)
countYearfLangAll acc [] _ = acc
countYearfLangAll (year,oopC,funcC,oopT,funcT) (x:xs) typeFlag = add4Tuple5 (countYearfLang year [] [] oopC funcC oopT funcT x typeFlag) (countYearfLangAll (year,oopC,funcC,oopT,funcT) xs typeFlag)


-- Function that counts the number of languages used by a user in each year
-- takes a year string, an accumulated list of oop and func languages to watch for duplicates,
-- also takes an accumulated count of unique foreign languages for both func and oop, foreign langugauge defined as a languages of a different type to user
-- also takes an accumulated count of total users of oop and func type informed by the typeFlag
-- returns a tuple containing year and the counts
countYearfLang :: String -> [String] -> [String] -> Float -> Float -> Float -> Float -> [String] -> Maybe Bool -> (String,Float,Float,Float,Float)
countYearfLang year uOop uFunc oopC funcC oopT funcT [] typeFlag
    | typeFlag == Nothing = (year,oopC,funcC,oopT,funcT)
    | typeFlag == Just False = (year,oopC,funcC,oopT,funcT+1)
    | typeFlag == Just True = (year,oopC,funcC,oopT+1,funcT)
    | otherwise = (year,oopC,funcC,oopT,funcT)
countYearfLang year uOop uFunc oopC funcC oopT funcT (x:y:z:xs) typeFlag
    | x == "oop" =
        if y `elem` uOop then
            countYearLang year uOop uFunc oopC funcC oopT funcT xs typeFlag
        else
            do
                let newTypeFlag = Just True
                let newuOop = uOop ++ [y]
                if z == year && not (isOop (pack y)) then
                    countYearfLang year newuOop uFunc (oopC+1) funcC oopT funcT xs newTypeFlag
                else
                    countYearfLang year uOop uFunc oopC funcC oopT funcT xs newTypeFlag
    | x == "functional" = 
        if y `elem` uFunc then
            countYearfLang year uOop uFunc oopC funcC oopT funcT xs typeFlag
        else
            do
                let newuFunc = uFunc ++ [y]
                let newTypeFlag = Just False
                if z == year && not (isFunc (pack y)) then
                    countYearfLang year uOop newuFunc oopC (funcC+1) oopT funcT xs newTypeFlag
                else
                    countYearfLang year uOop uFunc oopC funcC oopT funcT xs newTypeFlag
    | otherwise =  countYearfLang year uOop uFunc oopC funcC oopT funcT xs typeFlag


-- Function that takes list of commit data, list of years and a type flag that determines the type of the user.
-- Works recursively through the list adding an entry containg information about unique foreign languages encountered 
fLangCountAll :: (Float,Float,Float,Float) -> [[String]] -> Maybe Bool -> (Float,Float,Float,Float)
fLangCountAll tup [] _ = tup
fLangCountAll (oopC, funcC,oopT,funcT) (x:xs) typeFlag = add4Tuple (countfLangs [] [] oopC funcC oopT funcT x typeFlag) (fLangCountAll (oopC, funcC,oopT,funcT) xs typeFlag)


-- Function that counts the number of foreign languages used by a user
-- takes a year string, an accumulated list of oop and func languages to watch for duplicates,
-- also takes an accumulated count of unique foreign languages for both func and oop, foreign langugauge defined as a languages of a different type to user
-- also takes an accumulated count of total users of oop and func type informed by the typeFlag
-- returns a tuple containing year and the counts
countfLangs :: [String] -> [String] -> Float -> Float -> Float -> Float -> [String] ->  Maybe Bool -> (Float,Float,Float,Float)
countfLangs uOop uFunc oopC funcC oopT funcT [] typeFlag
    | typeFlag == Nothing = (oopC,funcC,oopT,funcT)
    | typeFlag == Just False = (oopC,funcC,oopT,funcT+1)
    | typeFlag == Just True = (oopC,funcC,oopT+1,funcT)
    | otherwise = (oopC,funcC,oopT,funcT)
countfLangs uOop uFunc oopC funcC oopT funcT (x:y:_:xs) typeFlag
  | x == "oop" =
        if y `elem` uOop
            then
                countfLangs uOop uFunc oopC funcC oopT funcT xs typeFlag
        else
            do
                let newuOop = uOop ++ [y]
                let newTypeFlag = Just True
                if not (isOop (pack y))
                    then
                        countfLangs newuOop uFunc (oopC+1) funcC oopT funcT xs newTypeFlag
                    else
                        countfLangs newuOop uFunc oopC funcC oopT funcT xs newTypeFlag
  | x == "functional" =
        if y `elem` uFunc
            then
                countfLangs uOop uFunc oopC funcC oopT funcT xs typeFlag
        else
            do
                let newuFunc = uFunc ++ [y]
                let newTypeFlag = Just False
                if not (isFunc (pack y))
                    then
                        countfLangs uOop newuFunc oopC (funcC+1) oopT funcT xs newTypeFlag
                    else
                        countfLangs uOop newuFunc oopC funcC oopT funcT xs newTypeFlag
  | otherwise = countfLangs uOop uFunc oopC funcC oopT funcT xs typeFlag

-- Function that takes list of commit data, list of years and a type flag that determines the type of the user.
-- Works recursively through the list adding an entry containg information about unique languages encountered 
langCountAll :: (Float,Float,Float,Float) -> [[String]] ->  Maybe Bool -> (Float,Float,Float,Float)
langCountAll tup [] _ = tup
langCountAll (oopC, funcC,oopT,funcT) (x:xs) typeFlag = add4Tuple (countLangs [] [] oopC funcC oopT funcT x typeFlag) (langCountAll (oopC, funcC,oopT,funcT) xs typeFlag)


-- Function that counts the number of foreign languages used by a user
-- takes a year string, an accumulated list of oop and func languages to watch for duplicates,
-- also takes an accumulated count of unique foreign languages for both func and oop, foreign langugauge defined as a languages of a different type to user
-- also takes an accumulated count of total users of oop and func type informed by the typeFlag
-- returns a tuple containing year and the counts
countLangs :: [String] -> [String] -> Float -> Float -> Float -> Float -> [String] -> Maybe Bool -> (Float,Float,Float,Float)
countLangs uOop uFunc oopC funcC oopT funcT [] typeFlag
    | typeFlag == Nothing = (oopC,funcC,oopT,funcT)
    | typeFlag == Just False = (oopC,funcC,oopT,funcT+1)
    | typeFlag == Just True = (oopC,funcC,oopT+1,funcT)
    | otherwise = (oopC,funcC,oopT,funcT)
countLangs uOop uFunc oopC funcC oopT funcT (x:y:_:xs) typeFlag
  | x == "oop" =
        if y `elem` uOop
            then
                countLangs uOop uFunc oopC funcC oopT funcT xs typeFlag
        else
            do
                let newTypeFlag = Just True
                let newuOop = uOop ++ [y]
                countLangs newuOop uFunc (oopC+1) funcC oopT funcT xs newTypeFlag
  | x == "functional" =
        if y `elem` uFunc
            then
                countLangs uOop uFunc oopC funcC oopT funcT xs typeFlag
        else
            do
                let newuFunc = uFunc ++ [y]
                let newTypeFlag = Just False
                countLangs uOop newuFunc oopC (funcC+1) oopT funcT xs newTypeFlag
  | otherwise = countLangs uOop uFunc oopC funcC oopT funcT xs typeFlag

-- Takes a nested list of 3 order Text tuples and recursively sorts the elements of each list
-- returns the list in the same order but each inside list is sorted
sortAll :: [[(Text,Text,Text)]] -> [[(Text,Text,Text)]] -> [[(Text,Text,Text)]]
sortAll sortedTup [] = sortedTup
sortAll sortedTup (x:xs) = sortedTup++[sortLTup x]++sortAll sortedTup xs

-- Takes a list list of 3 order Text tuples and sorts by the thrid element
--returns the list storted 
sortLTup :: Ord a => [(a,a,a)] -> [(a,a,a)]
sortLTup = sortBy (\(_,_,a) (_,_,b) -> compare a b)

-- Takes a nested list of Text and recursively each element to a a third order tuple of text appending the result on the accumulator
-- returns the accumulator when it is finished
tuplifyAll :: [[(Text,Text,Text)]] -> [[Text]] -> [[(Text,Text,Text)]]
tuplifyAll tup [] = tup
tuplifyAll tup (x:xs) = tup++[tuplify [] (splitText [] x)]++tuplifyAll tup xs

-- Takes a list of Text and converts each element to a a third order tuple of text
tuplify :: [(Text,Text,Text)] -> [Text] -> [(Text,Text,Text)]
tuplify tup [] = tup
tuplify tup (x:y:z:xs) = tup++[(x,y,z)]++tuplify tup xs

--Takes a list of Text and split recursively splits elements by comma value
-- Returns the list with more elements
splitText :: [Text] -> [Text] -> [Text]
splitText list [] = list
splitText list (x:xs) = list++DT.splitOn "," x++splitText list xs

-- Function that take a list of commit data and recursively isolates the year and appends it to an accumulator
--returns a list of the years
getActiveYears :: [Text] -> [(Text,Text,Text)] -> [Text]
getActiveYears acc [] = acc
getActiveYears acc ((_,_,year):xs) = acc++[year]++getActiveYears acc xs

-- takes two order 4 tuple of Float type and adds each corresponding element
add4Tuple :: (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> (Float,Float,Float,Float)
add4Tuple (x1,y1,z1,w1) (x2,y2,z2,w2) = (x1+x2,y1+y2,z1+z2,w1+w2)

-- takes two order 5 tuple of String Float type and adds each corresponding element excluding the first
add4Tuple5 :: (String,Float,Float,Float,Float) -> (String,Float,Float,Float,Float) -> (String,Float,Float,Float,Float)
add4Tuple5 (s1,x1,y1,z1,w1) (s2,x2,y2,z2,w2) = (s1,x1+x2,y1+y2,z1+z2,w1+w2)

-- isolates the first elem of a 3 elem tuple of uniform type
first3 :: (a,a,a) -> a
first3 (x,_,_) = x

-- isolates the second elem of a 3 elem tuple of uniform type
second3 :: (a,a,a) -> a
second3 (_,x,_) = x

-- isolates the third elem of a 3 elem tuple of uniform type
third3 :: (a,a,a) -> a
third3 (_,_,x) = x

-- isolates the first elem of a 4 elem tuple of uniform type
first4 :: (a,a,a,a) -> a
first4 (x,_,_,_) = x

-- isolates the second elem of a 4 elem tuple of uniform type
second4 :: (a,a,a,a) -> a
second4 (_,x,_,_) = x

-- isolates the third elem of a 4 elem tuple of uniform type
third4 :: (a,a,a,a) -> a
third4 (_,_,x,_) = x

-- isolates the fourth elem of a 4 elem tuple of uniform type
fourth4 :: (a,a,a,a) -> a
fourth4 (_,_,_,x) = x

-- takes a nested list of String elements and recursively converts them to Text
doubleListToText :: [[Text]] -> [[String]] -> [[Text]]
doubleListToText return [] = return
doubleListToText return (x:xs) = return++[listToText [] x]++doubleListToText return xs

-- takes a list of String elements and converts them to Text
listToText :: [Text] -> [String] -> [Text]
listToText = Prelude.foldl (\ t x -> t ++ [pack x])

-- takes a nested list of Text elements and recursively converts them to String
doubleListToString :: [[String]] -> [[Text]] -> [[String]]
doubleListToString return [] = return
doubleListToString return (x:xs) = return++[listToString [] x]++doubleListToString return xs

-- takes a list of Text elements and converts them to String
listToString :: [String] -> [Text] -> [String]
listToString = Prelude.foldl (\ t x -> t ++ [unpack x])

-- takes a nested list of Text elements and recursively Splits on a delimiter
doubleFlattenList :: [[Text]] -> [[Text]] -> [[Text]]
doubleFlattenList return [] = return
doubleFlattenList return (x:xs) = return++[flattenList [] x]++doubleFlattenList return xs

--Takes a list of Text and splits on "," values
flattenList :: [Text] -> [Text] -> [Text]
flattenList = Prelude.foldl (\ fList x -> fList ++ DT.splitOn "," x)


