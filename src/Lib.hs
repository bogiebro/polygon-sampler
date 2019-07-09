{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ConstraintKinds #-}
module Lib (polyInterior, polyExterior, getRegions, regionSample,
  RegionTy(..), Region(..), Seg(..), showPt, mconcatS,
  closestEdge, Realer
) where
import GHC.Generics (Generic)
import Control.DeepSeq
import Linear hiding (trace)
import Control.Lens hiding ((<|))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Semigroup
import Data.List
import Control.Monad
import Control.Monad.Random.Strict hiding (fromList)
import Text.Printf
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Semigroups
import Control.Arrow (first, (&&&), (***))
import Control.Exception
import Control.Monad.Trans.Maybe
import Debug.Trace

type Realer a = (Show a, RealFrac a, Floating a, RealFloat a)

type Range a = (a, a)
type YMap s a = Map (RPt a) (STSeg s a)

data Seg a = Seg {segStart :: !(V2 a), segEnd :: !(V2 a), segPolyOnBot :: !Bool, segSlope :: !a} deriving (Generic, NFData)
instance Realer a => Show (Seg a) where
  show (Seg a b c d) = showPt a ++ " ---> " ++ showPt b ++ " [slope=" ++ show d ++ " polyOnBot=" ++ show c ++ "]"
showPt p = printf "(%.2f,%.2f)" (realToFrac (p ^._x) :: Float) (realToFrac (p ^._y) :: Float)

-- Sampling from polygons is just weighted sampling from triangles and rectangles
data RegionTy = Rect | TopTri | BotTri deriving (Eq, Show, Generic, NFData)

-- Data for sampling within a given slice on the x axis. Probs are not normalized.
data Region a = Region {
  regionXRange :: Range a, regionYRange :: Range a, regionProb :: a,
  regionDiagSlope :: a, regionTy :: RegionTy} deriving (Show, Generic, NFData)

-- Indexes line segments along the y axis
data RPt a = RPt {rptY :: a, rptSlope :: a, rptPolyOnBot :: Bool} deriving (Eq, Ord, Show, Generic, NFData)

-- | STSegs keep a reference to their y coordinate over time
data STSeg s a = STSeg {stsegVal :: !(Seg a), stsegRPt :: !(STRef s (RPt a)), stsegX :: !(STRef s a)}
instance Realer a => Show (STSeg s a) where show (STSeg a _ _) = "ST " ++ show a

-- | Each line segment provides left and right endpoint events
data EventTy = L | R deriving (Eq, Ord, Show, Generic, NFData)

data Event s a = Event {
  eventSeg :: STSeg s a,
  eventPt :: V2 a,
  eventTy :: EventTy,
  eventSlope :: a,
  eventBot :: Bool}

instance Realer a => Show (Event s a) where
  show (Event s _ t _ _) = show t ++ " " ++ show s

eventOrder e = (eventPt e, eventSlope e, eventBot e)
instance Eq a => Eq (Event s a) where a == b = eventOrder a == eventOrder b
instance Ord a => Ord (Event s a) where compare a b = eventOrder a `compare` eventOrder b
  
event s@(STSeg (Seg start _ bot slope) _ _) L = Event s start L slope bot
event s@(STSeg (Seg _ end bot slope) _ _) R = Event s end R (-slope) bot

data SegGroup s a = SegGroup (V2 a) EventTy [(STSeg s a, YMap s a)]

handleGroupEvent :: Realer a => YMap s a -> Maybe (SegGroup s a) -> Event s a -> ST s (SegGroup s a, [Region a])
handleGroupEvent yMap Nothing (Event seg p2 ty2 _ _) = return (SegGroup p2 ty2 [(seg, yMap)], [])
handleGroupEvent yMap (Just g@(SegGroup p1 ty1 elems)) (Event seg p2 ty2 _ _)
  | p1 /= p2 = (SegGroup p2 ty2 [(seg, yMap)],) <$> makeRegions g
  | ty1 == ty2 = return (SegGroup p1 ty1 ((seg, yMap) : elems), [])
  | R <- ty1 = return (g, [])
  | R <- ty2 = return (SegGroup p2 ty2 [(seg, yMap)], [])

traceReg p n a = trace (show p ++ " (" ++ show n ++ "): " ++ unlines (map show a)) a

makeRegions :: Realer a => SegGroup s a -> ST s [Region a]
makeRegions (SegGroup pt ty l) = fmap (traceReg (pt, ty) (length l) . concat . concat) $ mapM makeRegion l where
  perhaps = MaybeT . return
  makeRegion (seg, yMap) = fmap maybeToList $ runMaybeT do
    trace ("Orig seg " ++ show seg) (return ())
    xval <- lift $ readSTRef (stsegX seg)
    rp <- lift $ readSTRef (stsegRPt seg)
    seg' <- perhaps $ oppositeSeg rp yMap
    trace ("Opposite seg " ++ show seg') (return ())
    x <- lift $ readSTRef (stsegX seg)
    x' <- lift $ readSTRef (stsegX seg')
    let !xrange = case ty of
                    L -> (x', pt ^._x)
                    R -> (max x x', pt ^._x)
    trace ("XRange " ++ show xrange) (return ())
    guard (fst xrange < snd xrange)
    lift $ writeSTRef (stsegX seg') (snd xrange) >> writeSTRef (stsegX seg) (snd xrange)
    (topTriRange, botTriRange) <- fmap (swapRpt rp) do
      opRange <- perhaps $ yrange xrange (stsegVal seg')
      case ty of
        L -> return ((pt ^._y, pt ^._y), opRange)
        R -> do
          myRange <- perhaps $ yrange xrange (stsegVal seg)
          return (myRange, opRange)
    let
        !(y3,y4) = orderedPair topTriRange
        !(y1,y2) = orderedPair botTriRange
        !dx = snd xrange - fst xrange
        dTopTriY = snd topTriRange - fst topTriRange
        dBotTriY = snd botTriRange - fst botTriRange
        dRectY = y3 - y2
        topTri = do
          guard (y3 /= y4)
          [Region xrange topTriRange (abs $ dx * dTopTriY / 2) (dTopTriY / dx) TopTri]
        rect = do
          guard (y2 /= y3)
          [Region xrange (y2, y3) (dRectY * dx) (dRectY / dx) Rect]
        botTri = do
          guard (y1 /= y2)
          [Region xrange botTriRange (abs $ dx * dBotTriY / 2) (dBotTriY / dx) BotTri]
    assert (y1 <= y2 && y2 <= y3 && y3 <= y4) $ return $ topTri ++ rect ++ botTri

-- Find the segment opposite the segment associated with this RPt
oppositeSeg :: Realer a => RPt a -> YMap s a -> Maybe (STSeg s a)
oppositeSeg rp@(rptPolyOnBot->True) m = do
  (rp', s) <- M.lookupGT rp m
  guard $ not (rptPolyOnBot rp')
  return s
oppositeSeg rp m = do
  (rp', s) <- M.lookupLT rp m
  guard $ rptPolyOnBot rp'
  return s

yrange :: Realer a => Range a -> Seg a -> Maybe (Range a)
yrange xrange s = do
  a <- valAt (fst xrange) s
  b <- valAt (snd xrange) s
  return (rptY a, rptY b)

swapRpt rp (a,b)
  | rptPolyOnBot rp = (b,a)
  | otherwise = (a,b)

data SweepState s a = SweepState {
  sweepMap :: YMap s a,
  sweepRegions :: [Region a],
  sweepGroup :: Maybe (SegGroup s a)}

-- Get the starting y-axis key for a segment
segRPt :: Seg a -> RPt a
segRPt (Seg p1 _ bot slope) = RPt (p1 ^._y) slope bot

getRegions :: Realer a => [Seg a] -> [Region a]
getRegions l = runST do
  events <- fmap (sort . concat) $ forM l $ \s-> do
    r <- newSTRef $ segRPt s
    x <- newSTRef $ (segStart s ^. _x)
    return $ map (event (STSeg s r x)) [L,R]
  state <- S.foldlM' handleEvent (SweepState M.empty [] Nothing) (S.fromList events)
  !lastRegions <- fmap concat . sequence $ fmap makeRegions (maybeToList $ sweepGroup state)
  return $ lastRegions ++ sweepRegions state
  
handleEvent :: Realer a => SweepState s a -> Event s a -> ST s (SweepState s a)
handleEvent (SweepState yMap regions group) e = do
  !yMap' <- handleYMapEvent yMap e
  (newGroup, !newRegions) <- handleGroupEvent yMap' group e
  return $ SweepState yMap' (newRegions ++ regions) (Just newGroup)

-- Adjust vertical order of line segments on encounering an endpoint
handleYMapEvent :: Realer a=> YMap s a -> Event s a -> ST s (YMap s a)
handleYMapEvent yMap (Event s@(STSeg a r _) _ L _ _) = do
  rp <- readSTRef r
  yMap' <- checkNeighbors (segStart a ^._x) rp yMap
  return $ M.insert rp s yMap'
handleYMapEvent yMap (Event (STSeg a r _) _ R _ _) = flip deleteParanoid yMap <$> readSTRef r

mconcatS :: (Monad m, Semigroup a) => S.Stream m a -> m a
mconcatS = S.foldl1' (<>)

regionSample :: (Realer a, Random a, MonadRandom m) => a -> [Seg a] -> [Region a] -> m a
regionSample k segs regions = fmap getAvg . mconcatS $ S.mapM f l where
  l = S.fromList regions
  f r = regionAvgDist segs k r

closestEdge :: Realer a => [Seg a] -> V2 a -> Arg a (Seg a)
closestEdge segs pt = getMin (foldl1 (<>) (fmap f segs)) where
  f = argMin (edgeDistance pt)

-- | Find the distance from a segment to a point
edgeDistance :: Realer a => V2 a -> Seg a -> a
edgeDistance pt (Seg v1 v2 _ _)
    | dir_len == 0 = norm (v1 - pt)
    | otherwise = norm (basept - closest)
  where
    dir = v2 - v1
    dir_len = norm dir
    norm_dir = dir ^/ dir_len
    basept = pt - v1
    proj_len = dot norm_dir basept
    proj = proj_len *^ norm_dir
    closest
      | proj_len > dir_len = dir
      | proj_len < 0 = V2 0 0
      | otherwise = proj

-- Get the average distance to a segment, given a normalizing constant for probability.
regionAvgDist :: (MonadRandom m, Random a, Realer a) => [Seg a] -> a -> Region a -> m (Avg a)
regionAvgDist segs n r = mconcatS l where
  l = S.replicateM (ceiling (p * n)) m
  m = (avg . getVal . closestEdge segs) <$> sampleRegion r
  p = regionProb r

-- Sample a point uniformly from the space defined by Region
sampleRegion :: (MonadRandom m, Random a, Realer a) => Region a -> m (V2 a)
sampleRegion (Region {..}) = do
  x <- getRandomR (orderedPair regionXRange)
  y <- getRandomR (orderedPair regionYRange)
  let dx = x - fst regionXRange
      dy = y - fst regionYRange
      sampleSlope = dy / dx
      topSwap = regionTy == TopTri && (
        (regionDiagSlope > 0 && sampleSlope > regionDiagSlope) ||
        (regionDiagSlope < 0 && sampleSlope > regionDiagSlope))
      botSwap = regionTy == BotTri && (
        (regionDiagSlope < 0 && sampleSlope < regionDiagSlope) ||
        (regionDiagSlope > 0 && sampleSlope < regionDiagSlope))
  return $ if topSwap || botSwap
    then V2 (flipCoord x regionXRange) (flipCoord y regionYRange)
    else V2 x y

orderedPair (a,b) = if a > b then (b,a) else (a,b)

-- Flip a number about the midpoint of the given range
flipCoord :: Realer a => a -> Range a -> a
flipCoord coord range = mid - (coord - mid) where mid = (fst range + snd range) / 2

-- | Convert a list of points traversed counter clockwise around a polygon to segments
polyInterior :: Realer a => [V2 a] -> [Seg a]
polyInterior l = toSegs' (head l) l where
  toSegs' a [b] = [seg b a]
  toSegs' a (x:y:l) = seg x y : toSegs' a (y:l)
  toSegs' _ _ = error "Invalid polygon"

polyExterior :: Realer a => [V2 a] -> [Seg a]
polyExterior = polyInterior . reverse

-- | Segments keep endpoints in order. 
seg p1 p2
    | p1 <= p2 = Seg p1 p2 False . checkNan $ slope (p2 - p1)
    | otherwise = Seg p2 p1 True . checkNan $ slope (p1 - p2)

-- | Segments with zero length are invalid
checkNan a
  | isNaN a = error "Invalid segment"
  | otherwise = a

-- | Get the slope of a vector
slope v = (v ^._y) / (v ^._x)

-- | Get the y for a particular x coordinate on a line, if it has one
valAt :: Realer a=> a -> Seg a -> Maybe (RPt a)
valAt x (Seg a b bot slope)
    | x < (a ^._x) || x > (b ^._x) = Nothing
    | isInfinite slope = if x == (a ^. _x) then Just (RPt (a ^._y) slope bot) else Nothing
    | otherwise = Just $ RPt ((a ^._y) + (x - (a ^._x)) * slope) slope bot

-- | Update the RPt associated with an STSeg at `v`
writeNewRPt :: Realer a=> STSeg s a -> RPt a -> RPt a -> YMap s a -> ST s (Map (RPt a) (STSeg s a))
writeNewRPt s@(STSeg seg ref _) k rp' yMap = do
  writeSTRef ref rp'
  return $ M.insert rp' s $ M.delete k yMap

-- | Check if a segment that started below us is now above
checkBelowST :: Realer a => a -> RPt a -> YMap s a -> ST s (YMap s a)
checkBelowST x rp yMap = fromMaybe (return yMap) do
  (k,s) <- M.lookupLT rp yMap
  v <- valAt x (stsegVal s)
  guard (v > rp)
  Just $ writeNewRPt s k v yMap >>= checkBelowST x rp

-- | Check if a segment that started above us is now below
checkAboveST :: Realer a => a -> RPt a -> YMap s a -> ST s (YMap s a)
checkAboveST x rp yMap = fromMaybe (return yMap) do
  (k,s) <- M.lookupGT rp yMap
  v <- valAt x (stsegVal s)
  guard (v < rp)
  Just $ writeNewRPt s k v yMap >>= checkAboveST x rp

-- | Check if neighbors need to be fast forwarded in time
checkNeighbors :: Realer a=> a -> RPt a -> YMap s a -> ST s (YMap s a)
checkNeighbors x rp ym = checkAboveST x rp ym >>= checkBelowST x rp

-- | Like M.delete, but throws an error if the key does not exist
deleteParanoid :: Ord k => k -> Map k v -> Map k v
deleteParanoid k m = assert (isJust (M.lookup k m)) $ M.delete k m
