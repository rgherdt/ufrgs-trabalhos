module Grasp (
      grasp
    ) where

import Data.Function (on)
import Data.Foldable (toList)
import Data.List
import qualified Data.Sequence as S
import Debug.Trace
import qualified Graph as G
import System.Random
import Data.Time

type Solution = S.Seq Int
type Cost = Int


-- | Return a random solution to the problem: a p-size list of locations.
-- The solution is already sorted for efficiency reasons.
randomSolution :: StdGen -> Int -> Int -> Solution
randomSolution gen n p =
    S.fromList . sort . take p . nub . randomRs (0, n - 1) $ gen

-- | Return all 1-change neighbours from @sol@.
neighbours :: Int -> Solution -> [Solution]
neighbours n sol = do
    pos <- allVertices
    nbs <- map (\v -> S.update pos v sol) emptyVertices
    return nbs
  where
    emptyVertices = allVertices \\ toList sol
    allVertices = [0 .. n - 1]

-- | Return all 2-change neighbours from @sol@.
{-
twoChangeNeighbours :: Int -> Solution -> [Solution]
twoChangeNeighbours n sol = do
    pos <- vs
    pos' <- delete pos vs
    nbs <- map (\v -> sol S.// [(pos, v)]) emptyVertices
    nbs' <- map (\v -> nbs S.// [(pos', v)]) $ emptyVertices \\ toList nbs
    return nbs'
  where
    vs = [0 .. S.length sol]
    emptyVertices = [0 .. n - 1] \\ toList sol
-}


solutionValue :: G.Graph -> Int -> Solution -> Int
solutionValue g n sol =
    sum [minimum [G.cost g i j | j <- s] | i <- [0 .. n - 1]]
  where 
    s = toList sol

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
    (S.fromList s, gen')
  where
    (s, _, gen') =
        foldr (\_ (sol, remaining, gen) ->
                  let size = alphaSize remaining
                      (j, gen') = randomIx gen size
                      v' = rcl size !! j
                  in (v' : sol, remaining - 1, gen'))
              ([], n, gen)
              [0 .. p - 1]
    vs = [0 .. n - 1]
    totalCost i = sum $ map (G.cost g i) vs
    candidates = sortBy (compare `on` snd) $ map (\v -> (v, totalCost v)) vs
    alphaSize n' = round (alpha * fromIntegral n') :: Int
    rcl size = map fst . take size $ candidates
    randomIx gen n' = randomR (0, n' - 1) gen


grasp :: StdGen
      -> G.Graph
      -> Int
      -> Int
      -> Float
      -> Int
      -> UTCTime
      -> IO (Int, Solution)
grasp gen g n p alpha counter0 startTime = go gen' counter0 val0 s0
  where
    (s0, gen') = randomizedGreedy gen g n p alpha
    val0 = solutionValue g n s0
    go gen counter val s
        | counter <= 0 = return (val, s)
        | val'' < val = do
            curTime <- getCurrentTime
            let diffTime = diffUTCTime curTime startTime
            putStrLn $ show val'' ++ "\t\t(" ++ show diffTime ++ ")"
            go gen' (counter - 1) val'' s''
        | otherwise  = go gen' (counter - 1) val s
      where
        (s', gen') = randomizedGreedy gen g n p alpha
        val' = solutionValue g n s'
        (val'', s'') = localSearch g (val', s')
--                         randomizedGreedy gen g n p alpha
    
