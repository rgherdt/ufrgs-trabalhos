module Grasp (
      StopCriterium (..)
    , grasp
    ) where

import Data.Array
import Data.Function (on)
import Data.List
import Debug.Trace
import qualified Graph as G
import System.Random

type Solution = Array Int Int
type Cost = Int

data StopCriterium = RelIter | AbsIter

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

-- | Return all 2-change neighbours from @sol@.
twoChangeNeighbours :: Int -> Solution -> [Solution]
twoChangeNeighbours n sol = do
    pos <- vs
    pos' <- delete pos vs
    nbs <- map (\v -> sol // [(pos, v)]) emptyVertices
    nbs' <- map (\v -> nbs // [(pos', v)]) $ emptyVertices \\ elems nbs
    return nbs'
  where
    vs = indices sol
    emptyVertices = [1 .. n] \\ elems sol


solutionValue :: G.Graph -> Int -> Solution -> Int
solutionValue g n sol =
    sum [minimum [G.cost g i j | j <- s] | i <- [1..n]]
  where 
    s = elems sol

-- | First improvement local search.
localSearch :: G.Graph -> (Cost, Solution) -> (Cost, Solution)
localSearch g (v0, s0) = case betters of
    [] -> (v0, s0)
    _  -> localSearch g (head betters)
  where
    betters = filter (\(v', _) -> v' < v0) $ map compute $ neighbours n s0
    n = G.numNodes g
    compute s = (solutionValue g n s, s)

randomizedGreedy :: StdGen -> G.Graph -> Int -> Int -> Float -> (Solution, StdGen)
randomizedGreedy gen g n p alpha =
    (listArray (1, p) s, gen')
  where
    (s, _, gen') =
        foldr (\_ (sol, remaining, gen) ->
                  let size = alphaSize remaining
                      (j, gen') = randomIx gen size
                      v' = rcl size !! j
                  in (v' : sol, remaining - 1, gen'))
              ([], n, gen)
              [1 .. p]
    vs = [1 .. n]
    totalCost i = sum $ map (G.cost g i) vs
    candidates = sortBy (compare `on` snd) $ map (\v -> (v, totalCost v)) vs
    alphaSize n' = round (alpha * fromIntegral n') :: Int
    rcl size = map fst . take size $ candidates
    randomIx gen n' = randomR (0, n' - 1) gen


grasp :: StdGen
      -> G.Graph
      -> StopCriterium
      -> Int
      -> Int
      -> Float
      -> Int
      -> (Int, Solution)
grasp gen g stop n p alpha counter0 = go gen counter0 val0 s0
  where
    s0 = randomSolution gen n p
    val0 = solutionValue g n s0
    go gen counter val s
        | counter <= 0 = (val, s)
        | val' < val = trace ("val': " ++ show val') $ case stop of
            RelIter -> go gen' counter0 val' s'
            _       -> go gen' (counter - 1) val' s'
        | otherwise  = go gen' (counter - 1) val s
      where
        (sr, gen') = randomizedGreedy gen g n p alpha
        (val', s') = localSearch g (solutionValue g n sr, sr)
--                         randomizedGreedy gen g n p alpha
    
