module Grasp (
      grasp,
      tst
    ) where

import Data.Function (on)
import Data.Foldable (toList)
import Data.List
import qualified Data.Sequence as S
import Debug.Trace
import qualified Graph as G
import System.Random
import Data.Time
import Data.Maybe

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


firstNeighbor :: Int -> Solution -> Solution
firstNeighbor n sol = S.update 1 (head emptyVertices) sol
  where
    emptyVertices = [0 .. n - 1] \\ toList sol

nextNeighbor :: Int -> Solution -> Solution -> Solution
nextNeighbor n sol nb
    | (diff == lev) && (pos == p) = S.empty
    | (diff == lev) = S.update (pos + 1) (head emptyVertices) sol
    | otherwise = S.update pos nextVert sol
  where
    p = (S.length sol) - 1
    emptyVertices = [0 .. n - 1] \\ toList sol
    lev = last emptyVertices
    diff = head ((toList nb) \\ (toList sol))
    diffPos = fromJust $ elemIndex diff emptyVertices
    nextVert = emptyVertices !! (diffPos+1)
    pos = fromJust $ S.elemIndexL diff nb
    
tst     :: G.Graph -> Int -> Int -> IO ()
tst g n p = do    
    let sol1 = S.fromList [(i*2) | i <- [0 .. p - 1]]
    --v1 = solutionValue g n sol1
    curTime <- getCurrentTime
    --let fn = firstNeighbor n sol1
    putStrLn "Calculate first Solution"
    let v0 = solutionValue g n sol1
    putStrLn "Local Search Start"
    let (val, sol) = localSearch2 g (v0, sol1)
    endTime <- getCurrentTime
    let diffTime = diffUTCTime curTime endTime
    putStrLn $ "(" ++ show diffTime ++ ") " ++ show val
    --t0 = trace(show fn ++ "\n") $ nextNeighbor n sol1 fn
    

localSearch2 :: G.Graph -> (Cost, Solution) -> (Cost, Solution)
localSearch2 g (v0, s0) 
    | firstVal < v0 = trace("nb: " ++ show fnb ++ "\n") $ localSearch2 g (firstVal, fnb)
    | S.null nnb    = (v0, s0)  
    | otherwise     = trace("nb: " ++ show nnb ++ "\n") $ localSearch2 g (nextVal, nnb)
  where
    n = G.numNodes g
    fnb = firstNeighbor n s0
    firstVal = solutionValue g n fnb
    (nextVal, nnb) = searchNeighbors g (v0, s0) (firstVal, fnb)


searchNeighbors :: G.Graph -> (Cost, Solution) -> (Cost, Solution) -> (Cost, Solution)
searchNeighbors g (v0, s0) (vnb, nb)
    | S.null next   = (-1, next)
    | nextVal < v0  = (nextVal, next)
    | otherwise     = searchNeighbors g (v0, s0) (nextVal, next)
  where 
    n = G.numNodes g
    next = nextNeighbor n s0 nb
    nextVal = solutionValue g n next

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
    
