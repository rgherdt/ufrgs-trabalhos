module Grasp (
      StopCriterium (..)
    , grasp
    , tst
    ) where

import Data.Array
import Data.Function (on)
import Data.List
import Debug.Trace
import qualified Graph as G
import System.Random
import Data.Time
import GHC.Exts
import Data.Maybe

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

firstNeighbor :: Int -> Solution -> Solution
firstNeighbor n sol = sol // [(1, head emptyVertices)]
  where
    emptyVertices = [1 .. n] \\ elems sol

nextNeighbor :: Int -> Int -> Solution -> Solution -> Solution
nextNeighbor n p sol nb = case diff of
    lev -> case pos of 
        p -> array (1,p) []
        _ -> sol // [((pos+1), head emptyVertices)]
    _ -> sol // [(pos, nextVert)]
  where
    emptyVertices = [1 .. n] \\ elems sol
    lev = trace("ev: "++ show emptyVertices)$last emptyVertices
    diff = head ((elems nb) \\ (elems sol))
    diffPos = fromJust $ elemIndex diff emptyVertices
    nextVert = emptyVertices !! (diffPos+1)
    (pos,_) = fromJust $ find (\(_,val) -> val == diff) $ assocs nb
    
tst :: [Int]
tst = trace(show t0) $ elems sol1
  where
    p1 = 5
    n1 = 8
    sol1 = array (1, 5) [(1,1),(2,3),(3,4),(4,7),(5,8)]
    fn = firstNeighbor n1 sol1
    t0 = trace(show fn ++ "\n") $ nextNeighbor n1 p1 sol1 fn
    t1 = trace(show t0 ++ "\n") $ nextNeighbor n1 p1 sol1 t0

-- | First improvement local search.
localSearch :: G.Graph -> (Cost, Solution) -> (Cost, Solution)
localSearch g (v0, s0) = case betters of
    [] -> (v0, s0)
    _  -> trace ("ls-res: " ++ show (fst (head betters))) $ localSearch g (head betters)
  where
    betters = sortWith (\(v',_) -> v') $ filter (\(v', _) -> v' < v0) $ map compute $ neighbours n s0
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
      -> UTCTime
      -> IO (Int, Solution)
grasp gen g stop n p alpha counter0 startTime = go gen counter0 val0 s0
  where
    s0 = randomSolution gen n p
    val0 = solutionValue g n s0
    go gen counter val s
        | counter <= 0 = return (val, s)
        | val' < val = do
            curTime <- getCurrentTime
            let diffTime = diffUTCTime curTime startTime
            putStrLn $ show val' ++ "\t\t(" ++ show diffTime ++ ")"
            case stop of
                RelIter -> go gen' counter0 val' s'
                _       -> go gen' (counter - 1) val' s'
        | otherwise  = go gen' (counter - 1) val s
      where
        (sr, gen') = randomizedGreedy gen g n p alpha
        valrs = solutionValue g n sr
        (val', s') = localSearch g (valrs, sr)
--                         randomizedGreedy gen g n p alpha
    
