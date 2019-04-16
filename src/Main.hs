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

    h <- fileHandler "debug.log" DEBUG >>= \lh -> return $
        setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    -- updateGlobalLogger "Kage" (addHandler h)
    updateGlobalLogger "Hadron" (setLevel INFO)

    let line = replicate 80 '─'
    infoM "Hadron" line

    runApplication "Hadron"

    infoM "Hadron" line
