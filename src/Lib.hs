--Stephen Cashin
--cashins

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    putStrLn "about to call"
    githubCall
    putStrLn "end."

githubCall :: IO ()
githubCall = do
    putStrLn "calling."