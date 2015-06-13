module Grasp where

import Data.Array
import Data.Function (on)
import Data.List
import qualified Graph as G
import System.Random
import System.Random.Shuffle (shuffle')

type Solution = Array Int Int

-- | Return a random solution to the problem: a p-size list of locations.
-- The solution is already sorted for efficiency reasons.
randomSolution :: StdGen -> Int -> Int -> Solution
randomSolution gen n p =
    listArray (1, p) . sort . take p . nub . randomRs (1, n) $ gen

-- | Return all 1-change neighbours from @sol@.
neighbours :: Int -> Solution -> [Solution]
neighbours n sol = do
    pos <- indices sol
    nbs <- map (\v -> sol // [(pos, v)]) emptyVertices
    return nbs
  where
    emptyVertices = [1 .. n] \\ elems sol

solutionValue :: G.Graph -> Int -> Solution -> Int
solutionValue g n sol =
    sum [minimum [G.cost g i j | j <- s] | i <- [1..n]]
  where 
    s = elems sol

-- | First improvement local search.
localSearch :: G.Graph -> Solution -> Solution
localSearch g s0 =
    snd . foldr comp (v0, s0) $ neighbours n s0
  where
    n = G.numNodes g
    dim = bounds s0
    v0 = solutionValue g n s0
    comp s' best@(v, s) | v' < v = (v', s')
                        | otherwise = best
      where
        v' = solutionValue g n s'

randomizedGreedy :: StdGen -> G.Graph -> Int -> Int -> Float -> Maybe Solution
randomizedGreedy gen g n p a
    | num < p = Nothing
    | otherwise = Just $ listArray (1, p) $ shuffle' rcl p gen
  where
    vs = [1 .. n]
    totalCost i = sum $ map (G.cost g i) vs
    candidates = sortBy (compare `on` snd) $ map (\v -> (v, totalCost v)) vs
    num = round (a * fromIntegral n) :: Int
    rcl = map fst . take num $ candidates

