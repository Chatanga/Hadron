module Common.Debug
    ( trace
    , _trace
    , traceList
    , _traceList
    ) where

import Data.List

import qualified Debug.Trace as Trace

trace :: Show a => String -> a -> a
trace t a = Trace.trace (t ++ " = " ++ show a) a
_trace _ = id

traceList :: Show a => String -> [a] -> [a]
traceList t as = Trace.trace (t ++ " = [\n\t" ++ intercalate "\n\t" (map show as) ++ "\n]") as
_traceList _ = id
