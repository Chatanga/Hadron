{-# language ScopedTypeVariables, TypeFamilies, FlexibleContexts, TupleSections #-}

module Graphics.MarchingCube
    ( EdgeIndice
    , cube
    , cubeEdges
    , cubeFaces
    , maxCellTriangleCount
    , generateCaseProtoTriangleList
    )
where

import Prelude hiding ((.), id, (<*))
import Control.Category (Category((.)), id)
import Data.Int (Int8, Int32)
import Data.List ((\\), elemIndex, groupBy, intersect, nub, partition, sort, sortBy)
import Data.Maybe (fromJust, maybe)
import Data.Ord ( comparing )
import Data.Tuple (swap)
import Graphics.GPipe

import Common.Debug

----------------------------------------------------------------------------------------------------------------------

type VerticeIndice = Int
type EdgeIndice = Int
type FaceIndice = Int

cube :: Num a => [V3 a]
cube =
    [ V3 0 0 0
    , V3 1 0 0
    , V3 0 1 0
    , V3 1 1 0
    , V3 0 0 1
    , V3 1 0 1
    , V3 0 1 1
    , V3 1 1 1
    ]

type Edge = (VerticeIndice, VerticeIndice)

-- directed edges actually
cubeEdges :: [Edge] =
    [ (0, 1), (0, 2), (0, 4) -- <- 0
    , (1, 0), (1, 3), (1, 5) -- <- 3
    , (2, 0), (2, 3), (2, 6) -- <- 6
    , (3, 1), (3, 2), (3, 7) -- <- 9
    , (4, 0), (4, 5), (4, 6) -- <- 12
    , (5, 1), (5, 4), (5, 7) -- <- 15
    , (6, 2), (6, 4), (6, 7) -- <- 18
    , (7, 3), (7, 5), (7, 6) -- <- 21
    ]

{-
        5 4
        |/
    3---+---1
       /|
      2 0
-}
faces :: [FaceIndice] = [0 .. 5]

-- Coordinates are sorted for each face for comparison, not rendering.
cubeFaces :: [(VerticeIndice, VerticeIndice, VerticeIndice, VerticeIndice)]
cubeFaces =
    [ (0, 1, 2, 3)
    , (1, 3, 5, 7)
    , (2, 3, 6, 7)
    , (0, 2, 4, 6)
    , (0, 1, 4, 5)
    , (4, 5, 6, 7)
    ]

folds :: [({- left -} FaceIndice, {- right -} FaceIndice)] -- !! EdgeIndice -- when looking inside the cube oriented along the edge
folds =
    [ (0, 4), (3, 0), (4, 3) -- <- 0
    , (4, 0), (0, 1), (1, 4) -- <- 3
    , (0, 3), (2, 0), (3, 2) -- <- 6
    , (1, 0), (0, 2), (2, 1) -- <- 9
    , (3, 4), (4, 5), (5, 3) -- <- 12
    , (4, 1), (5, 4), (1, 5) -- <- 15
    , (2, 3), (3, 5), (5, 2) -- <- 18
    , (1, 2), (5, 1), (2, 5) -- <- 21
    ]

getLeftFace :: EdgeIndice -> FaceIndice
getLeftFace = fst . (folds !!)

getRightFace :: EdgeIndice -> FaceIndice
getRightFace = snd . (folds !!)

-- Reorder a shore’s edges into a proper convex polygon.
toConvexPolygon :: Maybe FaceIndice -> [EdgeIndice] -> [EdgeIndice]
toConvexPolygon _ [] = []
toConvexPolygon Nothing (e:es) = e : toConvexPolygon (Just (getRightFace e)) es
toConvexPolygon (Just expectedLeftFaceIndice) es =
    let ([edgeToTheRight], otherEdges) = partition ((expectedLeftFaceIndice ==) . getLeftFace) es
    in  edgeToTheRight : toConvexPolygon (Just (getRightFace edgeToTheRight)) otherEdges

toConvexPolygon' :: Maybe FaceIndice -> [Edge] -> [EdgeIndice]
toConvexPolygon' f es = toConvexPolygon f eis where
    edgeIndex = zipWith (flip (,)) [0 ..] cubeEdges
    eis = map (\edge -> fromJust (lookup edge edgeIndex)) es

maxCellTriangleCount = 5

-- case -> list of proto triangles of edge indices
generateCaseProtoTriangleList :: [Bool] -> [(EdgeIndice, EdgeIndice, EdgeIndice)]
generateCaseProtoTriangleList aboveness | length (_traceList "aboveness" aboveness) == 8 = triangles' where
    {-
         4+----+5
         /|   /|
       6+-|--+7|
        |0+--|-|1
        |/   |/
       2+----+3
    -}
    vertices :: [VerticeIndice] = [0 .. 7]

    (innerEdges, transitionEdges) = partition (\(i1, i2) -> aboveness !! i1 == aboveness !! i2) cubeEdges

    -- Partition the vertices in connected graphs where all vertices are above (the isles) or below (the seas).
    isInnerConnected i =
        let neighbours = map snd (filter ((==) i . fst) (_traceList "innerEdges" innerEdges))
        in  not . null . intersect neighbours
    (isles, seas) = partition ((aboveness !!) . head) (findConvexSubGraphs isInnerConnected vertices)

    findConvexSubGraphs :: (a -> [a] -> Bool) -> [a] -> [[a]]
    findConvexSubGraphs _ [] = []
    findConvexSubGraphs isConnected (x:xs) = let
        classes = partition (isConnected x) (findConvexSubGraphs isConnected xs)
        in case classes of
            ([], ocs) -> [x]:ocs
            (cs, ocs) -> (x:concat cs):ocs

    findShoreEdges area = filter ((`elem` area) . fst) (_traceList "transitionEdges" transitionEdges)
    isleShoreEdges = map findShoreEdges (_traceList "isles" isles)
    seaShoreEdges = map findShoreEdges (_traceList "seas" seas)

    -- It’s complicated…
    triangles
        | length (filter id aboveness) <= 4 = case (isleShoreEdges, seas) of
            ([_], [_]) -> weaveSurface isleShoreEdges
            ([isleShore1, isleShore2], [sea]) -> weaveTunnel (_traceList "sea" sea) (_traceList "isleShore1" isleShore1) (_traceList "isleShore2" isleShore2)
            (_, _) -> weaveSurface (map (map swap) seaShoreEdges)
        | length isles > length seas = weaveSurface isleShoreEdges
        {-
        When an isle has shores leading to many seas, normalizing them could be complicated.
        Even if it is not the case, the result could be different when isles and seas are
        reversed. seems easier to select the more fragmented part, isles or sea, and change
        the edge directions for the latter.
        -}
        | otherwise = weaveSurface (map (map swap) seaShoreEdges)

    triangles' = if length triangles <= 5
        then triangles
        else weaveSurface isleShoreEdges -- error $ "Too many triangles: " ++ show (length triangles)

generateCaseProtoTriangleList _ = error "aboveness must contains data for 8 vertices"

weaveSurface :: [[Edge]] -> [(EdgeIndice, EdgeIndice, EdgeIndice)]
weaveSurface shoreEdges = triangles where
    toConvexPolygons = map (toConvexPolygon' Nothing) shoreEdges

    triangles =
        let ts = _traceList "triangles" $ concatMap toTriangleList (_traceList "toConvexPolygons" toConvexPolygons)
        in  if length ts <= maxCellTriangleCount * 3 then ts else error "too many vertices generated"

    toTriangleList ps
        | length ps <= maxCellTriangleCount + 3 = toTriangleList' ps
        | otherwise = error $ "Too many edges: " ++ show (length ps)

    toTriangleList' :: [a] -> [(a, a, a)]
    toTriangleList' [] = []
    toTriangleList' (p:ps) = zipWith (\pN pM -> (p, pN, pM)) ps (tail ps)

type IndexedEdge = (Int, Edge)
type Entry = (VerticeIndice, IndexedEdge)

data Face = TriangleFace Bool Edge Edge Edge | QuadFace Edge Edge Edge Edge deriving Show
isTriangleFace TriangleFace {} = True
isTriangleFace _ = False

weaveTunnel :: [VerticeIndice] -> [Edge] -> [Edge] -> [(EdgeIndice, EdgeIndice, EdgeIndice)]
weaveTunnel sea isleShore1 isleShore2 = triangles' where
    entries :: [Entry]
    entries = map (\(i, e) -> (snd e, (i, e))) (map (1,) isleShore1 ++ map (2,) isleShore2)

    seaEdges = filter (\(e1, e2) -> e1 `elem` sea && e2 `elem` sea) cubeEdges

    links :: [(Edge, Edge)]
    links = map (\((1, e1), (2, e2)) -> (e1, e2)) (step 2 entries)

    collectLinks :: [(VerticeIndice, IndexedEdge)] -> [(IndexedEdge, IndexedEdge)]
    collectLinks es = nub $ concatMap (product . partition ((==) 1 . fst)) (merge es)

    merge :: Ord k => [(k, v)] -> [[v]]
    merge es = map (map snd) (groupBy (\x y -> fst x == fst y) (sortBy (comparing fst) es))

    product :: ([a], [b]) -> [(a, b)]
    product (s1, s2) = [ (a, b) | a <- s1, b <- s2 ]

    step :: Integer
        -> [Entry]
        -> [(IndexedEdge, IndexedEdge)]
    step n es = if n == 0 then [] else ls ++ step (n-1) es'' where
        ls = collectLinks es

        linkedEdges = concatMap (\(a, b) -> [a, b]) ls
        es' = filter ((`notElem` linkedEdges) . snd) es

        propagateEdgeBySea (v, ie) = (v, ie) : map (\e -> (snd e, ie)) (filter ((==) v . fst) seaEdges)
        es'' = concatMap propagateEdgeBySea es'

    isle1 = map (cubeEdges!!) (toConvexPolygon' Nothing isleShore1)
    isle2 = map (cubeEdges!!) (toConvexPolygon' Nothing isleShore2)

    next :: [Edge] -> Bool -> Edge -> Edge
    next isle False edge = next (reverse isle) True edge
    next isle True edge = head $ tail $ dropWhile (not . (==) edge) (cycle isle)

    weave :: (Edge, Edge) -> (Edge, Edge) -> [Face]
    weave firstLink (a, b) = allTriangles where
        (a', b') = (next isle1 True a, next isle2 False b)
        (triangles, nextLink)
            | (a, b') `elem` links = ([TriangleFace False b' b a], (a, b'))
            | (a', b) `elem` links = ([TriangleFace True a a' b], (a', b))
            | (a', b') `elem` links = ([QuadFace a a' b b'], (a', b'))
            | otherwise = error "Should not happen!"
        allTriangles = if nextLink /= firstLink
            then triangles ++ weave firstLink nextLink
            else triangles

    toTriangles2 :: Bool -> [Face] -> [(Edge, Edge, Edge)]
    toTriangles2 s' (TriangleFace s i1 i2 i3 : faces) = if s == s'
        then (i1, i2, i3) : toTriangles2 (not s') faces
        else error "Inconsistent triangle strip!"
    toTriangles2 s (QuadFace a a' b b' : faces) = (if s
        then [(a, a', b), (a', b', b)]
        else [(a, a', b'), (a, b', b)]) ++ toTriangles2 (not s) faces
    toTriangles2 _ [] = []

    toTriangles :: [Face] -> [(Edge, Edge, Edge)]
    toTriangles faces = case break isTriangleFace faces of
        (quads, []) -> concatMap (\(QuadFace a a' b b') -> [(a, a', b), (a', b', b)]) quads
        (quads, TriangleFace s i1 i2 i3 : otherFaces) -> toTriangles2 s (TriangleFace s i1 i2 i3 : otherFaces ++ quads)
        _ -> error "Should not happen!"

    isCubeFace' :: Face -> Bool
    isCubeFace' TriangleFace {} = False
    isCubeFace' (QuadFace e1 e2 e3 e4) = case nub . sort . concatMap (\e -> [fst e, snd e]) $ [e1, e2, e3, e4] of
        [i1, i2, i3, i4] -> (i1, i2, i3, i4) `elem` cubeFaces
        _ -> False

    isCubeFace :: (Edge, Edge, Edge) -> Bool
    isCubeFace (e1, e2, e3) = case nub . sort . concatMap (\e -> [fst e, snd e]) $ [e1, e2, e3] of
        [i1, i2, i3, i4] -> (i1, i2, i3, i4) `elem` cubeFaces
        _ -> False

    triangles =
        let firstLink = head links
        in  filter (not . isCubeFace) (toTriangles (filter (not . isCubeFace') (weave firstLink firstLink)))

    toEdgeIndice edge = case edge `elemIndex` cubeEdges of
            Just i -> i
            Nothing -> error $ "Edge not found: " ++ show edge
    triangles' = map (\(e1, e2, e3) -> (toEdgeIndice e1, toEdgeIndice e2, toEdgeIndice e3)) triangles
