module Graphics.Layouts
    ( fixedLayout
    , SplitLayoutOrientation(..)
    , splitLayout
    , BordelLayoutPosition(..)
    , borderLayout
    , AnchorConstraint(..)
    , anchorLayout
    , adaptativeLayout
    ) where

import Data.Maybe
import Data.Tree

import Graphics.View

------------------------------------------------------------------------------------------------------------------------

getWidth :: Tree (View m a) -> Int
getWidth = fst . snd . viewLocalBounds . rootLabel

getHeight :: Tree (View m a) -> Int
getHeight = snd . snd . viewLocalBounds . rootLabel

setBounds position size tree = layoutSetSize (viewLayout (rootLabel tree')) size tree' where
    tree' = setPosition position tree

fixedLayout (w, h) = Layout
    (const (Just w, Just h))
    setSize

{-
         Horizontal (0.3)
    ┏━━━━━━━━┯━━━━━━━━━━━━━━━━━┓
    ┃        │                 ┃
    ┃        │                 ┃
    ┃        │                 ┃
    ┃        │                 ┃
    ┃        │                 ┃
    ┃  left  │      right      ┃
    ┃        │                 ┃
    ┃        │                 ┃
    ┃        │                 ┃
    ┃        │                 ┃
    ┃        │                 ┃
    ┗━━━━━━━━┷━━━━━━━━━━━━━━━━━┛
-}

data SplitLayoutOrientation = Vertical | Horizontal deriving (Eq, Show)

getHeight' c = snd (layoutGetNaturalSize (viewLayout (rootLabel c)) c)
getWidth' c = snd (layoutGetNaturalSize (viewLayout (rootLabel c)) c)

sum' xs = if null xs then Nothing else Just (sum xs)
maximum' xs = if null xs then Nothing else Just (maximum xs)

splitLayout :: SplitLayoutOrientation -> Float -> Layout m a
splitLayout Vertical ratio = Layout getter setter where
    getter t = (maximum' (mapMaybe getWidth' (subForest t)), sum' (mapMaybe getHeight' (subForest t)))
    setter (w, h) t =
        let [up, down] = subForest t
            fh = floor (fromIntegral h * ratio)
            up' = setBounds (0, 0) (w, fh) up
            down' = setBounds (0, fh) (w, h - fh) down
        in  setSize (w, h) t{ subForest = [up', down'] }
splitLayout Horizontal ratio = Layout getter setter where
    getter t = (sum' (mapMaybe getWidth' (subForest t)), maximum' (mapMaybe getHeight' (subForest t)))
    setter (w, h) t =
        let [left, right] = subForest t
            fw = floor (fromIntegral w * ratio)
            left' = setBounds (0, 0) (fw, h) left
            right' = setBounds (fw, 0) (w - fw, h) right
        in  setSize (w, h) t{ subForest = [left', right'] }

{-
    ┏━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃           top            ┃
    ┠─────┬──────────────┬─────┨
    ┃     │              │     ┃
    ┃     │              │     ┃
    ┃     │              │     ┃
    ┃left │    center    │right┃
    ┃     │              │     ┃
    ┃     │              │     ┃
    ┃     │              │     ┃
    ┠─────┴──────────────┴─────┨
    ┃          bottom          ┃
    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━┛
-}

data BordelLayoutPosition = Center | Top | Left | Bottom | Right deriving (Eq, Show)

borderLayout :: [BordelLayoutPosition] -> Layout m a
borderLayout positions = undefined -- TODO

{-
    ┏━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃               ▲          ┃
    ┃               │ top      ┃
    ┃               ▼          ┃
    ┃           ┌───────┐      ┃
    ┃    left   │       │ right┃
    ┃◀---------▶│       │◀----▶┃
    ┃           │       │      ┃
    ┃           └───────┘      ┃
    ┃               ▲          ┃
    ┃               │ bottom   ┃
    ┃               ▼          ┃
    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━┛
-}

data AnchorConstraint = AnchorConstraint
    { topDistance :: Maybe Int
    , rightDistance :: Maybe Int
    , bottomDistance :: Maybe Int
    , leftDistance :: Maybe Int
    } deriving Show

anchorLayout :: [AnchorConstraint] -> Layout m a
anchorLayout constraints = Layout getter setter where
    getter t = (Nothing, Nothing)
    setter s t = setSize s t{ subForest = zipWith updateChild constraints (subForest t) } where
        (w, h) = s
        updateChild constraint child = setBounds (xChild, yChild) (wChild, hChild) child where
            (mWidth, mHeigh) = layoutGetNaturalSize (viewLayout (rootLabel child)) child
            (xChild, wChild) = case (leftDistance constraint, mWidth, rightDistance constraint) of
                (Nothing, _, Nothing) -> let iw = fromMaybe w mWidth in ((w - iw) `div` 2, iw) -- centrage
                (Just l, _, Nothing) -> (l, w') where w' = fromMaybe (w - l) mWidth
                (Nothing, _, Just r) -> (w - w' - r, w') where w' = fromMaybe (w - r) mWidth
                (Just l, _, Just r) -> (l, w - l - r) -- La taille naturelle du composant est ignorée.
            (yChild, hChild) = case (topDistance constraint, mHeigh, bottomDistance constraint) of
                (Nothing, _, Nothing) -> let ih = fromMaybe h mHeigh in ((h - ih) `div` 2, ih) -- centrage
                (Just t, _, Nothing) -> (t, h') where h' = fromMaybe (h - t) mHeigh
                (Nothing, _, Just b) -> (h - h' - b, h') where h' = fromMaybe (h - b) mHeigh
                (Just t, _, Just b) -> (t, h - t - b) -- La taille naturelle du composant est ignorée.

{-
    ┏━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃                          ┃
    ┃           9x27           ┃
    ┃         (master)         ┃
    ┃                          ┃
    ┃                          ┃
    ┃                          ┃
    ┃                          ┃
    ┃                          ┃
    ┠────────┬────────┬────────┨
    ┃  3x9   │  3x9   │  3x9   ┃
    ┃(slave) │(slave) │(slave) ┃
    ┗━━━━━━━━┷━━━━━━━━┷━━━━━━━━┛
-}

adaptativeLayout :: Layout m a
adaptativeLayout = Layout getter setter where
    getter t = case subForest t of
        [] -> (Nothing, Nothing)
        (master : slaves) ->
            let n = fromIntegral (length slaves)
                (mw, mh) = layoutGetNaturalSize (viewLayout (rootLabel master)) master
            in  if n > 0
                then (mw, (\h -> h + h `div` n) <$> mh)
                else (mw, mh)
    setter s t = setSize s t' where
        t' = case subForest t of
            [] -> t
            (master : slaves) ->
                let n = fromIntegral (length slaves)
                in  if n > 0
                    then
                        let
                            (w, h) = s
                            wMaster = w
                            hMaster = (n * h) `div` (n + 1)
                            wSlave = w `div` n
                            hSlave = h - hMaster
                            master' = setBounds (0, 0) (wMaster, hMaster) master
                            updateSlave index = setBounds (wSlave * index, hMaster) (wSlave, hSlave)
                        in
                        t{ subForest = master' : zipWith updateSlave [0..] slaves }
                    else
                        t{ subForest = [setBounds (0, 0) s master] }
