{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad      (forM_)
import           System.Environment (getArgs)

import           Regex

main :: IO ()
main = do
    regexStr <- unwords <$> getArgs
    case parseRegex regexStr of
        Nothing -> putStrLn "can't parse regex"
        Just regex -> do
            input <- getContents
            forM_ (lines input) $ \line ->
                if checkRegex regex line
                    then putStrLn line
                    else pure ()
