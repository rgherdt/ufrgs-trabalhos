module Grasp (
      grasp
    ) where

import Control.Monad.ST
import Data.Function (on)
import Data.Foldable (toList)
import Data.List
import qualified Data.IntMap as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Debug.Trace
import qualified Graph as G
import System.Random
import Data.Time
import Data.STRef

type Solution = V.Vector Int
type Cost = Int
inf = (maxBound :: Int)

-- | Return a random solution to the problem: a p-size list of locations.
-- The solution is already sorted for efficiency reasons.
randomSolution :: StdGen -> Int -> Int -> Solution
randomSolution gen n p =
    V.fromList . sort . take p . nub . randomRs (0, n - 1) $ gen

-- | Return all 1-change neighbours from @sol@.
neighbours :: Int -> Solution -> [Solution]
neighbours n sol = do
    pos <- solIndices
    nbs <- map (\v -> sol V.// [(pos, v)]) candidates
    return nbs
  where
    candidates = [0 .. n - 1] \\ toList sol
    solIndices = [0 .. (V.length sol - 1)]

-- | Return all 2-change neighbours from @sol@.
{-
twoChangeNeighbours :: Int -> Solution -> [Solution]
twoChangeNeighbours n sol = do
    pos <- vs
    pos' <- delete pos vs
    nbs <- map (\v -> sol V.// [(pos, v)]) emptyVertices
    nbs' <- map (\v -> nbs V.// [(pos', v)]) $ emptyVertices \\ toList nbs
    return nbs'
  where
    vs = [0 .. V.length sol]
    emptyVertices = [0 .. n - 1] \\ toList sol
-}


solutionValue :: G.Graph -> Int -> Solution -> Int
solutionValue g n sol =
    sum [minimum [G.cost g i j | j <- s] | i <- [0 .. n - 1]]
  where 
    s = toList sol

{-
nearestFacility :: G.Graph -> Solution -> Int -> Int
nearestFacility g sol i =
    foldr findMin inf (toList sol)
  where
    findMin j curMin
        | c < curMin = c
        | otherwise = curMin
      where
        c = G.cost g i j
-}
    

-- | Return a tuple containing the nearest and the second nearest facilities
-- to node @i@.
nearestFacilities :: G.Graph -> Solution -> Int -> (Int, Int)
nearestFacilities g sol i =
    (nearest, sndNearest)
  where
    ((_, nearest), (_, sndNearest)) = 
        foldr findNearest ((inf, sol V.! 0), (inf, sol V.! 0)) (toList sol)
    findNearest j cur@((fstCost', fstNearest'), (sndCost', sndNearest'))
        | c <= fstCost' = ((c, j), (fstCost', fstNearest'))
        | otherwise = cur
      where
        c = G.cost g i j

-- | Return a tuple containing the nearest and the second nearest facilities
-- to every node in the graph.
allNearestFacilities :: G.Graph -> Int -> Solution -> V.Vector (Int, Int)
allNearestFacilities g n sol =
    V.fromList $ map (nearestFacilities g sol) [0 .. n - 1]


findOut :: G.Graph -> Solution -> Int -> (Cost, Int)
findOut g sol i = (profit, leaving)
  where
    n = G.numNodes g
    remaining = V.fromList $ [0 .. n - 1] \\ toList sol
    (gaining, losing) =
        V.partition
            (\u -> G.cost g u i <= G.cost g u (fst $ nearestVec V.! u))
            remaining
    nearestVec = allNearestFacilities g n sol -- is a vector of pairs
    gain = V.sum $
            V.map (\u -> G.cost g u (fst $ nearestVec V.! u) - G.cost g u i)
                  gaining
    netloss u = let nearest = nearestVec V.! u in
                min (G.cost g u i) (G.cost g u (snd $ nearest))
                - G.cost g u (fst $ nearest)
    netlossMap =
        foldr (\u acc -> M.adjust (netloss u +) (fst $ nearestVec V.! u) acc)
              (M.fromList . map (\v -> (v, 0)) $ toList sol)
              (toList losing)
    (leaving, loss) = head . sortBy (compare `on` snd) . M.toAscList $ netlossMap
    profit = gain - loss

optLocalSearch :: G.Graph -> Solution -> Solution
optLocalSearch g s
    | profit best >= 0 = trace (show index) $ optLocalSearch g (s V.// [(index, incoming best)])
    | otherwise = s
  where
    Just index = V.findIndex (leaving best ==) s
    n = G.numNodes g
    candidates :: [(Int, (Cost, Int))]
    candidates = sortBy (compare `on` profit)
                 . map (\i -> (i, findOut g s i)) $ [0 .. n - 1] \\ toList s
    best = head candidates
    incoming = fst
    profit = fst . snd
    leaving = snd . snd
    

{-
optLocalSearch :: G.Graph -> Int -> Int -> Solution -> Solution
optLocalSearch g n p sol = go (0, 0) sol nearest
  where
    go :: (Int, Int) -> Solution -> V.Vector Int -> Solution
    go cur@(i, j) sol nearest
        | diff < 0 = go (0, 0) (sol V.// [(i, incoming V.! j)]) nearest'
        | next cur == cur = sol
        | otherwise = go (next cur) sol nearest
      where
        oldNode = sol V.! i
        newNode = incoming V.! j
        relevant = filter (\k -> oldNode == (nearest V.! k)) indices
        diff = foldr (\k acc -> (G.cost g newNode k - G.cost g oldNode k) + acc) 0 relevant
        nearest' = fmap (\k -> if k == oldNode then newNode else k) nearest 
        incoming = V.fromList $ indices \\ toList sol
    indices = [0 .. n - 1]
    nearest = nearestFacilities g n sol
    next cur@(i, j) | i < p - 1 && j < n - p - 1 = (i, j + 1)
                    | i < p - 1 && j == n - p - 1 = (i + 1, 0)
                    | j < n - p - 1 = (i, j + 1)
                    | otherwise = cur
-}

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
    (V.fromList s, gen')
  where
    (s, _, gen') =
        foldr (\_ (sol, remaining, gen) ->
                  let size = alphaSize (length remaining)
                      (j, gen') = randomIx gen size
                      v' = rcl size remaining !! j
                  in (v' : sol, delete v' remaining, gen'))
              ([], vs, gen)
              [0 .. p - 1]
    vs = [0 .. n - 1]
    totalCost i = sum $ map (G.cost g i) vs
    candidates vs = sortBy (compare `on` snd) $ map (\v -> (v, totalCost v)) vs
    alphaSize n' = round (alpha * fromIntegral n') :: Int
    rcl size remaining = map fst . take size $ candidates remaining
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
--    s0 = randomSolution gen n p 
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
--        (val'', s'') = localSearch g (val', s')
        s'' = optLocalSearch g s'
        val'' = solutionValue g n s''
    
