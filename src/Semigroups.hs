{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Semigroups (Avg, avg, getAvg, Std, getStd, std, argMin, getVal, getArg) where
import Data.Semigroup

newtype Avg = Avg (Sum Float, Sum Int) deriving (Eq, Show, Semigroup)

getAvg :: Avg -> Float
getAvg (Avg (a,n)) = getSum a / fromIntegral (getSum n)

avg :: Float -> Avg
avg = Avg . (,Sum 1) . Sum

newtype Std = Std (Avg, [Float]) deriving (Eq, Show, Semigroup)

getStd :: Std -> Float
getStd (Std (mu,l)) = getAvg $ foldr1 (<>) $ map (avg . abs . subtract (getAvg mu)) l

std :: Float -> Std
std a = Std (avg a, [a])

argMin :: (b -> a) -> b -> Min (Arg a b)
argMin f x = Min (Arg (f x) x)

getVal (Arg a _) = a
getArg (Arg _ a) = a
