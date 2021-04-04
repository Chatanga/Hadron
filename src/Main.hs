module Main where

import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Graphics.Application

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    {-
    print $ generateCaseProtoTriangleList [False, True, False, False, False, False, False, True]
    print $ generateCaseProtoTriangleList [True, True, False, True, False, True, True, True]
    -}

    {-
    let sea = [0, 3, 4, 6, 7]
        isle1 = [6, 7, 8]
        isle2 = [16, 17, 3, 4]
    print $ weaveTunnel sea isle1 isle2

    let sea = [0, 1, 4, 5, 6, 7]
        isle1 = [6, 7, 8]
        isle2 = [3, 4, 5]
    print $ weaveTunnel sea isle1 isle2

    let sea = [0, 1, 3, 4, 6, 7]
        isle1 = [6, 7, 8]
        isle2 = [15, 16, 17]
    print $ weaveTunnel sea isle1 isle2
    -}

    if False
        then do
            h <- fileHandler "debug.log" DEBUG >>= \lh -> return $
                setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
            updateGlobalLogger "Hadron" (addHandler h)
        else
            updateGlobalLogger "Hadron" (setLevel INFO)

    let line = replicate 80 '─'
    infoM "Hadron" line

    runApplication "Hadron"

    infoM "Hadron" line
