{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Graph where

import Control.Monad (when)
import Control.Monad (mzero, guard, forM, forM_)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray as MArray
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Monoid 
data MGraph = MGraph (IOUArray (Int, Int) Cost) -- ^ a mutable graph
data Graph = Graph (Array (Int, Int) Cost)      -- ^ an immutable graph
type Cost = Int
type Edge = ((Int, Int), Cost)
inf = (maxBound :: Int)


------------ Immutable functions -------------

-- | Return the number of nodes in graph @g@.
numNodes :: Graph -> Int
numNodes (Graph g) = n
  where
    (_, (n, _)) = bounds g

-- | Return the element (cost) stored at position @i@, @j@ in graph @g@.
cost :: Graph -> Int -> Int -> Cost
cost (Graph g) i j = g ! (i, j)

-- | Process one input line, in the format of a list [src, tgt, cost].
-- If the input line is malformed, return Nothing. Otherwise, return the
-- corresponding edge.
edgeFromList :: [Int] -> Maybe Edge
edgeFromList [src, tgt, cost] = Just ((src, tgt), cost)
edgeFromList _ = Nothing

showInput :: Int -> Int -> Graph -> B.ByteString
showInput n p gr@(Graph g) =
--    g <- freeze m :: IO (Array (Int, Int) Cost)
    "data;\n"
    <> "param p := " <> B8.pack (show p) <> ";\n"
    <> "param n := " <> B8.pack (show n) <> ";\n"
    <> "param d : \t"
    <> B.concat (map (("\t" <>) . B8.pack . show) nodes) <> " :=\n"
    <> B.concat (map (\i -> ("\t\t\t" <> B8.pack (show i)
                       <> B.concat (map (\j -> "\t"
                            <> B8.pack (show (g ! (i, j)))) nodes)
                       <> "\n"))
               nodes)
    <> "\t;\nend;"
  where
    nodes = [1 .. n]

-- | Return an immutable graph from a mutable one.
freeze :: MGraph -> IO Graph
freeze (MGraph m) =
    return . Graph =<< (MArray.freeze m :: IO (Array (Int, Int) Cost))


----------- Mutable functions --------------

-- | Process all input lines. If any input line is malformed, return Nothing.
-- Otherwise return the corresponding graph.
fromList :: Int -> [[Int]] -> MaybeT IO MGraph
fromList n input = do
    edges <- maybe mzero return $ sequence . map edgeFromList $ input
    m <- lift $ newListArray ((1, 1), (n, n)) $ repeat inf -- fill with inf values
    lift $ do
        mapM_ (\i -> writeArray m (i, i) 0) [1..n]      -- zeroes main diagonal
        mapM_ (\((i, j), c) -> writeArray m (i, j) c >> writeArray m (j, i) c)
              edges
    return . MGraph $ m

-- | Computes a graph containing the shortest path from every node. Assumes
-- all edges have a cost associated.
shortestPath :: MGraph -> IO ()
shortestPath gr@(MGraph m) = do
    (_, (n, _)) <- getBounds m 
    let indices = [1..n]
    forM_ indices $ \i -> do
        forM_ indices $ \j -> do
            c <- readArray m (i, j)
            if c < inf
            then forM_ indices $ \k -> do
                c' <- readArray m (j, k)
                if c' < inf
                then do
                     let s   = c + c'
                     old <- readArray m (k, i)
                     if s < old
                     then do writeArray m (k, i) s
                             writeArray m (i, k) s
                     else return ()
                else return ()
            else return ()
    return ()


