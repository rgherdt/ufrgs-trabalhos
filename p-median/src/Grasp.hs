module Grasp where

import Data.Array
import Data.List
import System.Random

type Solution = Array Int Int

-- | Return a random solution to the problem: a p-size list of locations.
-- The solution is already sorted for efficiency reasons.
randomSolution :: StdGen -> Int -> Int -> Solution
randomSolution gen n p =
    listArray (1, p) . sort . take p . nub . randomRs (1, n) $ gen

neighbours :: Int -> Solution -> [Solution]
neighbours n sol = do
    pos <- indices sol
    nbs <- map (\v -> sol // [(pos, v)]) emptyVertices
    return nbs
  where
    emptyVertices = [1 .. n] \\ elems sol

solutionValue :: Graph -> Solution -> Int
solutionValue g sol =
    sum [minimum [G.cost g i j | j <- s] | i <- [1..n]]
  where 
    n = G.numNodes g
    s = elems sol
