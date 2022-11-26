module Main where

import Control.Monad
import Data.Time.Clock
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Graphics.Application
import Graphics.Font

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    if False
        then do
            h <- fileHandler "debug.log" DEBUG >>= \lh -> return $
                setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
            updateGlobalLogger "Hadron" (addHandler h)
        else
            updateGlobalLogger "Hadron" (setLevel DEBUG)

    when False $ do
        t0 <- getCurrentTime
        -- createDistanceFieldAtlas "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
        createDistanceFieldAtlas "/usr/share/fonts/truetype/noto/NotoMono-Regular.ttf"
        t1 <- getCurrentTime
        infoM "Hadron" $ "Atlas font generated in: " ++ show (diffUTCTime t1 t0)

    let line = replicate 80 '─'
    infoM "Hadron" line

    runApplication "Hadron"

    infoM "Hadron" line
