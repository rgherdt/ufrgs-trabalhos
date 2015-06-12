module Grasp where

import Data.List
import System.Random

type Solution = [Int]

-- | Return a random solution to the problem: a p-size list of locations.
-- The solution is already sorted for efficiency reasons.
randomSolution :: StdGen -> Int -> Int -> Solution
randomSolution gen n p = sort . take p . nub . randomRs (1, n) $ gen

