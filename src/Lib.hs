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
--takes a name parameter to select a user

someFunc :: IO ()
someFunc = do
    writeFile "cPie.csv" ""
    writeFile "cBar.csv" ""
    writeFile "typeCount.csv" ""
    writeFile "totalLang.csv" ""
    writeFile "totalfLang.csv" ""
    writeFile "yearLang.csv" ""
    writeFile "yearfLang.csv" ""
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
            formatLang "cBar.csv" "totalLang.csv" "totalfLang.csv" "yearLang.csv" "yearfLang.csv"

    else
        do
            print "not file"
            githubCall auth uName
            formatTypeCount "cPie.csv" "typeCount.csv"
            formatLang "cBar.csv" "totalLang.csv" "totalfLang.csv" "yearLang.csv" "yearfLang.csv"

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


getUser :: GH.User -> Maybe Text
getUser (GH.User _ Nothing _) = Nothing
getUser (GH.User _ n _) = n

interleave :: [(a,b)] -> [a] -> [b] -> [(a,b)]
interleave list _ [] =  list
interleave list [] _ = list
interleave list (x:xs) (y:ys) = list++[(x,y)]++interleave list xs ys

getLang ::GH.Repo -> Maybe Text
getLang (GH.Repo _ Nothing) = Nothing
getLang (GH.Repo _ l) = l

getUserCommits :: BasicAuthData -> IO SC.ClientEnv -> Maybe Text -> Text -> [(String,Maybe Text,String,String)] -> [(String,Maybe Text)] -> IO ()
getUserCommits _ _ user name acc [] = do
                                                let userCommits = removeNonUserCommits user name [] acc
                                                --print userCommits
                                                let sorteduserCommits = sortBy (\(_,_,_,a) (_,_,_,b) -> compare a b) userCommits
                                                let earlyUserCommits = shortenedUC sorteduserCommits
                                                let pType = determinePType 0 0 0 earlyUserCommits
                                                exportUserCommits pType userCommits
                                                appendFile "cPie.csv" (pType++"\n")
                                                --print earlyUserCommits
                                                print pType


getUserCommits auth env user name acc ((repo,lang):xs) = (SC.runClientM (GH.getCommits (Just "haskell-app") auth name (pack repo)) =<< env) >>= \case
                                                Left err -> do
                                                    putStrLn $ "Problem getting commits: " ++ show err
                                                Right commits -> do
                                                    let authorList =  map (\xx -> showCommit xx repo lang) commits
                                                    let newAcc = acc++authorList
                                                    getUserCommits auth env user name newAcc xs


showCommit ::  GH.Commit -> String -> Maybe Text -> (String,Maybe Text,String,String)
showCommit (GH.Commit sha commitA) = showCommitA commitA

showCommitA :: GH.CommitA -> String -> Maybe Text -> (String,Maybe Text,String,String)
showCommitA (GH.CommitA author) = showAuthor author

showAuthor :: GH.Author -> String -> Maybe Text -> (String,Maybe Text,String,String)
showAuthor (GH.Author name email date) repo lang = (repo, lang, unpack name, unpack date)

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

exportUserCommits :: String -> [(String,Maybe Text,String,String)] -> IO ()
exportUserCommits _ [] = appendFile "cBar.csv" ("---" ++"\n")
exportUserCommits pType ((repo,Nothing,uName,date):xs) = exportUserCommits pType xs
exportUserCommits pType ((repo,Just lang,uName,date):xs) = do
                                                    appendFile "cBar.csv" (pType ++ ","++ unpack lang ++ "," ++ Prelude.take 4 date ++"\n" )
                                                    exportUserCommits pType xs


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

getLangType ::  Text -> String
getLangType lang
    | isFunc lang = "functional"
    | isOop lang = "oop"
    | otherwise = "other"

class IsFunc a where
    isFunc :: a -> Bool

instance IsFunc (Maybe Text) where
    isFunc Nothing = False
    isFunc (Just lang) = do
                    let funcLang = ["Haskell","Clean","Scala","Erlang","Clojure","SML","F#","Scheme","XSLT","SQL","Mathematica","Elixir","Elm","PureScript","Racket","Reason","Swift","Nix","Emacs","Lua","TSQL","Emacs Lisp","R"]
                    unpack lang `elem` funcLang

instance IsFunc Text where
    isFunc lang = do
                    let funcLang = ["Haskell","Clean","Scala","Erlang","Clojure","SML","F#","Scheme","XSLT","SQL","Mathematica","Elixir","Elm","PureScript","Racket","Reason","Swift","Nix","Emacs","Lua","TSQL","Emacs Lisp","R"]
                    unpack lang `elem` funcLang

class IsOop a where
    isOop :: a -> Bool

instance IsOop (Maybe Text) where
    isOop Nothing = False
    isOop (Just lang) = do
        let oopLang = ["Python","C","C++","Java","TypeScript","Go","Ruby","C#","Visual Basic .Net","Perl","Dart","Kotlin","CommonLisp","MATLAB","Samlltalk","Groovy","Powershell", "Objective-C","ActionScript"]
        unpack lang `elem` oopLang

instance IsOop Text where
    isOop lang = do
        let oopLang = ["Python","C","C++","Java","TypeScript","Go","Ruby","C#","Visual Basic .Net","Perl","Dart","Kotlin","CommonLisp","MATLAB","Samlltalk","Groovy","Powershell", "Objective-C","ActionScript"]
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
                                    "oop," ++ show (first3 typeCount) ++ "\n" ++
                                    "functional," ++ show (second3 typeCount) ++ "\n" ++
                                    "other," ++ show (third3 typeCount))

countTypes :: Int -> Int -> Int -> [String] -> (Int,Int,Int)
countTypes oopC funcC otherC [] = (oopC,funcC,otherC)
countTypes oopC funcC otherC (x:xs)
    | x == "oop" = countTypes (oopC+1) funcC otherC xs
    | x == "functional" = countTypes oopC (funcC+1) otherC xs
    | x == "other" = countTypes oopC funcC (otherC+1) xs

first3 :: (a,a,a) -> a
first3 (x,_,_) = x

second3 :: (a,a,a) -> a
second3 (_,x,_) = x

third3 :: (a,a,a) -> a
third3 (_,_,x) = x

formatLang :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO()
formatLang input output foutput youtput fyoutput = do
                            i <- openFile input ReadMode
                            contents <- hGetContents i
                            let lData = DS.splitOn ["---"] (Prelude.lines contents)
                            let tlData = doubleListToText [] lData
                            --print lData
                            let tupData = sortAll [] (tuplifyAll [] tlData)
                            --print tupData
                            let flatData = doubleFlattenList [] tlData
                            --print flatData
                            let activeYears = sort (nub (getActiveYears [] (concat tupData)))
                            print activeYears
                            let langCounts = langCountAll (0,0,0,0) (doubleListToString [] flatData) Nothing
                            print langCounts
                            let flangCounts = fLangCountAll (0,0,0,0) (doubleListToString [] flatData) Nothing
                            
                            print flangCounts
                            appendFile output ("type,lang" ++ "\n" ++
                                "oop," ++ show (first4 langCounts/third4 langCounts) ++ "\n" ++
                                "functional," ++ show (second4 langCounts/fourth4 langCounts) ++ "\n")
                            appendFile foutput ("type,lang" ++ "\n" ++
                                "oop," ++ show (first4 flangCounts/third4 flangCounts) ++ "\n" ++
                                "functional," ++ show (second4 flangCounts/fourth4 flangCounts) ++ "\n")

--countYearLangs :: [String] -> [(String,Float,Float,Float,Float)] -> [String] -> [(String,Float,Float,Float,Float)]

sortAll :: [[(Text,Text,Text)]] -> [[(Text,Text,Text)]] -> [[(Text,Text,Text)]]
sortAll sortedTup [] = sortedTup
sortAll sortedTup (x:xs) = sortedTup++[sortLTup x]++sortAll sortedTup xs

sortLTup :: Ord a => [(a,a,a)] -> [(a,a,a)]
sortLTup = sortBy (\(_,_,a) (_,_,b) -> compare a b)

tuplifyAll :: [[(Text,Text,Text)]] -> [[Text]] -> [[(Text,Text,Text)]]
tuplifyAll tup [] = tup
tuplifyAll tup (x:xs) = tup++[tuplify [] (splitText [] x)]++tuplifyAll tup xs

tuplify :: [(Text,Text,Text)] -> [Text] -> [(Text,Text,Text)]
tuplify tup [] = tup
tuplify tup (x:y:z:xs) = tup++[(x,y,z)]++tuplify tup xs

splitText ::[Text] -> [Text] -> [Text]
splitText list [] = list
splitText list (x:xs) = list++DT.splitOn "," x++splitText list xs

getActiveYears :: [Text] -> [(Text,Text,Text)] -> [Text]
getActiveYears acc [] = acc
getActiveYears acc ((_,_,year):xs) = acc++[year]++getActiveYears acc xs

fLangCountAll :: (Float,Float,Float,Float) -> [[String]] -> Maybe Bool -> (Float,Float,Float,Float) 
fLangCountAll tup [] _ = tup
fLangCountAll (oopC, funcC,oopT,funcT) (x:xs) typeFlag = add4Tuple (countfLangs [] [] oopC funcC oopT funcT x typeFlag) (fLangCountAll (oopC, funcC,oopT,funcT) xs typeFlag)

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

langCountAll :: (Float,Float,Float,Float) -> [[String]] ->  Maybe Bool -> (Float,Float,Float,Float)
langCountAll tup [] _ = tup
langCountAll (oopC, funcC,oopT,funcT) (x:xs) typeFlag = add4Tuple (countLangs [] [] oopC funcC oopT funcT x typeFlag) (langCountAll (oopC, funcC,oopT,funcT) xs typeFlag)

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

add4Tuple :: (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> (Float,Float,Float,Float)
add4Tuple (x1,y1,z1,w1) (x2,y2,z2,w2) = (x1+x2,y1+y2,z1+z2,w1+w2)

first4 :: (a,a,a,a) -> a
first4 (x,_,_,_) = x

second4 :: (a,a,a,a) -> a
second4 (_,x,_,_) = x

third4 :: (a,a,a,a) -> a
third4 (_,_,x,_) = x

fourth4 :: (a,a,a,a) -> a
fourth4 (_,_,_,x) = x

doubleListToText :: [[Text]] -> [[String]] -> [[Text]]
doubleListToText return [] = return
doubleListToText return (x:xs) = return++[listToText [] x]++doubleListToText return xs

listToText :: [Text] -> [String] -> [Text]
listToText = Prelude.foldl (\ t x -> t ++ [pack x])

doubleListToString :: [[String]] -> [[Text]] -> [[String]]
doubleListToString return [] = return
doubleListToString return (x:xs) = return++[listToString [] x]++doubleListToString return xs

listToString :: [String] -> [Text] -> [String]
listToString = Prelude.foldl (\ t x -> t ++ [unpack x])

doubleFlattenList :: [[Text]] -> [[Text]] -> [[Text]]
doubleFlattenList return [] = return
doubleFlattenList return (x:xs) = return++[flattenList [] x]++doubleFlattenList return xs

flattenList :: [Text] -> [Text] -> [Text]
flattenList = Prelude.foldl (\ fList x -> fList ++ DT.splitOn "," x)


