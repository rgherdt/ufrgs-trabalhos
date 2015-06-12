{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Graph as G
import System.IO
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

inf = maxBound :: Int

main = do
    params <- liftM (map read . words) getLine :: IO [Int]
    matrix <- case params of
        [n, numEdges, p] -> do
            contents <- return . B8.lines =<< B8.getContents
            g <- runMaybeT . G.generateGraph n $
                     map (map read . map B8.unpack . B8.words) contents
            case g of
                Just g -> do
                    B8.putStrLn $ G.showInput n p g
                    return ()
                _ -> B8.putStrLn "p-median: Inconsistent input graph"
            return ()
        _ -> fail "p-median: First input line wrong formatted."
    return ()
