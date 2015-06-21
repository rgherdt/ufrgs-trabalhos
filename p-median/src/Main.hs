{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Control.Monad (liftM)
import qualified Graph as G
import Grasp (grasp, StopCriterium (..))
import System.IO
import System.Random
import System.CPUTime
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

data Options = Options
    { optNum :: Int
    , optTime :: Bool
    , optAlpha :: Float
    }


parseOptions :: Parser Options
parseOptions = Options
    <$> option auto ( short 'n'
                   <> value 1000
                   <> metavar "NUM"
                   <> help "Number of iterations to stop (default: 100)."
                    )
    <*> switch ( short 't'
              <> long "time"
              <> help "If true, uses time (set by -n) as stop criterium"
               )
    <*> option auto ( long "alpha"
                   <> short 'a'
                   <> value 0.2
                   <> metavar "ALPHA"
                   <> help "Alpha parameter in (0.0, 1.0] (default: 0.2)"
                    )

opts = info (helper <*> parseOptions)
            ( fullDesc
           <> progDesc "Solve the p-median uncapacitaded problem with GRASP"
           <> header "A GRASP-based solver for the p-median uncapacitaded problem" )
            
main = do
    op <- execParser opts
    let iterNum = optNum op
        alpha = optAlpha op
        stop | optTime op = TimeStop
             | otherwise = IterStop
    params <- liftM (map read . words) getLine :: IO [Int]
    matrix <- case params of
        [n, numEdges, p] -> do
            contents <- return . B8.lines =<< B8.getContents
            gen <- getStdGen
            let g = G.generateGraph n $
                     map (map read . map B8.unpack . B8.words) contents
            case g of
                Just g -> do
                    startTime <- getCPUTime
                    putStrLn $ "solution\trunning time"
                    (val, s) <- grasp gen g n p alpha iterNum stop startTime
                    return ()
                _ -> B8.putStrLn "p-median: Inconsistent input graph"
            return ()
        _ -> fail "p-median: First input line wrong formatted."
    return ()

