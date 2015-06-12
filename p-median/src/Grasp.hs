module Grasp where

import Data.List
import System.Random

-- | Return a random solution to the problem: a p-size list of locations.
-- The resulting list is already sorted.
firstSolution :: StdGen -> Int -> Int -> [Int]
firstSolution gen n p = sort . take p . nub . randomRs (1, n) $ gen

