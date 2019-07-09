{-# LANGUAGE NoMonomorphismRestriction #-}
module TestLib where
import Control.Lens
import Linear hiding (trace)
import Linear.Affine
import Lib
import Test.QuickCheck hiding (scale)
import Test.QuickCheck.Gen
import qualified Test.QuickCheck.Monadic as QM
import Data.Maybe
import Data.Semigroup
import Data.Eq.Approximate
import TypeLevel.NaturalNumber
import Control.Monad.Random
import qualified Data.Map.Strict as M
import System.Directory
import Data.Aeson
import Data.List
import GHC.Float
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Diagrams.Prelude as D
import Diagrams.Backend.Rasterific
import Semigroups
import Test.Tasty.QuickCheck
import Test.QuickCheck.Gen (chooseAny)
import Data.Colour.Palette.RandomColor
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Debug.Trace
import qualified Data.List.NonEmpty as N
import Numeric.AD
import Numeric.AD.Internal.Reverse
import Data.Reflection
import Data.Functor.Compose

instance MonadRandom Gen where
  getRandomR = choose
  getRandom = chooseAny
  getRandomRs a = infiniteListOf (getRandomR a)
  getRandoms = infiniteListOf chooseAny

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary UUID where arbitrary = chooseAny

middle :: [V2 Float]
middle = map (uncurry V2) [(0,0), (1, 0), (1, 1), (0, 1)]
bigger :: [V2 Float]
bigger = map (uncurry V2) [(-1,-1), (2, -1), (2,2), (-1, 2)]

toPt (Seg p1 p2 b _) = P $ fmap float2Double $ if b then p2 else p1

toTrail :: [D.P2 Double] -> D.Located (D.Trail V2 Double)
toTrail = D.mapLoc D.closeTrail .  D.fromVertices

drawSegs :: UUID -> [[Seg Float]] -> IO ()
drawSegs uuid s = drawDiag uuid (segDiag s)

segDiag :: [[Seg Float]] -> D.Diagram B
segDiag s = (lbls <> diag) where
  diag = foldMap D.strokeLocTrail (map toTrail pts) & D.lw D.veryThin
  lbls = foldMap toLbl (nub $ concat pts)
  pts = map (map toPt) s

drawDiag :: UUID -> D.Diagram B -> IO ()
drawDiag uuid diag = do
  createDirectoryIfMissing False "errplots"
  renderRasterific ("errplots/" ++ UUID.toString uuid ++ ".png") (D.mkWidth 3000) $
    D.pad 1.1 $ D.centerXY diag

drawPtSegs :: UUID -> [[Seg Float]] -> V2 Float -> IO ()
drawPtSegs uuid s p = drawDiag uuid $
  (D.circle 0.1 & D.translate (fmap float2Double p) & D.fc D.red & D.lw D.none) <> segDiag s

toLbl :: D.P2 Double -> D.Diagram B
toLbl (P p) = showPt p & texterific & D.scale 0.1 & D.translate p

drawRegionData :: MonadRandom m => Region Float -> m (D.Diagram B)
drawRegionData r
  = do
    color <- randomCIELab
    return $ D.strokeLocTrail (toTrail l) & D.lw D.veryThin & D.fc color
  where
    l = map (fmap float2Double) pts
    pts = if regionTy r == Rect then [a,b,c,d] else [c,e,a]
    a = D.p2 (fst (regionXRange r), fst (regionYRange r))
    b = D.p2 (snd (regionXRange r), fst (regionYRange r))
    c = D.p2 (snd (regionXRange r), snd (regionYRange r))
    d = D.p2 (fst (regionXRange r), snd (regionYRange r))
    e = case (regionTy r, regionDiagSlope r > 0) of
          (TopTri, True) -> D.p2 (snd (regionXRange r), fst (regionYRange r))
          (TopTri, False) -> D.p2 (fst (regionXRange r), snd (regionYRange r))
          (BotTri, True) -> D.p2 (fst (regionXRange r), snd (regionYRange r))
          (BotTri, False) -> D.p2 (snd (regionXRange r), fst (regionYRange r))

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> S.Stream m a -> m b
foldMapM f = mconcatS . S.mapM f

type PolyData = ([[Seg Float]], [Region Float])

toPolyData :: Real a => [[Seg a]] -> PolyData
toPolyData seglist = (seglist', getRegions segs) where
  seglist' = map (map coerceSeg) seglist
  segs = concat seglist'

coerceSeg (Seg a b bot slope) = Seg (fmap realToFrac a) (fmap realToFrac b) bot (realToFrac slope)

drawRegions :: PolyData -> IO (D.Diagram B)
drawRegions (segs, regions) = evalRandIO do
  overlay <- foldMapM drawRegionData (S.fromList $ regions)
  return $ segDiag segs <> overlay

wrapAD :: Float -> AbsolutelyApproximateValue (Digits Three) Float
wrapAD = AbsolutelyApproximateValue

traceReg :: [Region Float] -> [Region Float]
traceReg l = trace (unlines (map show l)) l

instance Reifies s Tape => Random (Reverse s Float) where
  random g = let (a, g') = random g
             in (auto a, g')
  randomR (x,y) g = let (a, g') = randomR (realToFrac x, realToFrac y) g
                in (auto a, g')

-- outdoorScore :: Realer a => StdGen -> [V2 Float] -> (Compose (Compose [] []) V2 a) -> a
outdoorScore g (Compose exterior) (Compose (Compose pts)) = flip evalRand g m where
  segs = concat $ polyExterior exterior : map polyInterior pts
  m = regionSample 500 segs (getRegions segs)

midF = Compose (Compose [middle])
midG = fmap auto midF
biggerG = fmap auto (Compose bigger)

-- foo :: Reifies s Tape => Compose (Compose [] []) V2 (Reverse s Float) -> Reverse s Float
-- foo (Compose (Compose (x:_))) = getVal $ closestEdge (polyInterior x) (V2 (-0.5) (-0.5))

-- propgrad = total $ trace (show $ grad foo midG) (2::Int)

prop_sampler = total $ trace (show $ grad (outdoorScore (mkStdGen 2) biggerG) midG) (2::Int)

-- We get NaNs again! Dang. But at least stuff compiles.
