module Common.Debug
    ( traceIt
    , _traceIt
    , traceList
    , _traceList
    ) where

import Data.List

import qualified Debug.Trace as Trace

traceIt :: Show a => String -> a -> a
traceIt t a = Trace.trace (t ++ " = " ++ show a) a
_traceIt _ = id

traceList :: Show a => String -> [a] -> [a]
traceList t as = Trace.trace (t ++ " = [\n\t" ++ intercalate "\n\t" (map show as) ++ "\n]") as
_traceList _ = id
