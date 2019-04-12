{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Semigroups (Avg, avg, getAvg, Std, getStd, std, argMin, getVal, getArg) where
import Data.Semigroup

newtype Avg a = Avg (Sum a, Sum Int) deriving (Eq, Show, Semigroup)

getAvg :: Fractional a => Avg a -> a
getAvg (Avg (a,n)) = getSum a / fromIntegral (getSum n)

avg :: a -> Avg a
avg = Avg . (,Sum 1) . Sum

newtype Std a = Std (Avg a, [a]) deriving (Eq, Show, Semigroup)

getStd :: Fractional a => Std a -> a
getStd (Std (mu,l)) = getAvg $ foldr1 (<>) $ map (avg . abs . subtract (getAvg mu)) l

std :: a -> Std a
std a = Std (avg a, [a])

argMin :: (b -> a) -> b -> Min (Arg a b)
argMin f x = Min (Arg (f x) x)

getVal (Arg a _) = a
getArg (Arg _ a) = a
