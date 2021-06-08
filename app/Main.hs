{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.TSP ( CostFunc )
import Data.TSPLIB95 ( readTSP )
import Control.SimulatedAnnealing.TSP
import qualified Data.Map as M
-- import qualified Data.List as L (sort)
-- import Data.Maybe
-- import qualified Data.Text as T
-- import Data.Ord (comparing)
-- import Data.Function (on)
import System.Random (mkStdGen)
import qualified Data.Sequence as S
import Control.Monad.State (evalState)
import Data.Foldable (toList)
-- import System.Directory
-- import System.Environment  
import Options.Applicative
import Debug.Trace

data Options = Options {
    filePath :: FilePath,
    maxEpochs :: Int,
    maxTemp :: Double,
    minTemp :: Double,
    seed :: Int
} deriving (Show)

filePathParser :: Parser FilePath
filePathParser = strOption (
    long "file" <>
    metavar "PATH" <>
    help "Run Simulated Annealing on a Travelling Salesmen Problem specified by the path of a file in the TSPLIB95 format." )

seedParser :: Parser Int
seedParser = option auto (
    long "seed" <>
    metavar "INT" <>
    value 42 <>
    help "Seed for the pseudo-random number generator")

maxEpochsParser :: Parser Int
maxEpochsParser = option auto (
    long "max_epochs" <>
    metavar "INT" <>
    help "Maximum number of iterations to run the solver")

maxTempParser :: Parser Double
maxTempParser = option auto (
    long "max_temp" <>
    metavar "NUM" <>
    help "Maximum temperature of the simulation")

minTempParser :: Parser Double
minTempParser = option auto (
    long "min_temp" <>
    metavar "NUM" <>
    value 0 <>
    help "Minimum temperature of the simulation")

-- versionParser :: Parser (a -> a)
-- versionParser = infoOption "0.0.0" (long "version" <> help "Display version")

optionsParser :: Parser Options
optionsParser = Options <$> filePathParser <*> maxEpochsParser <*> maxTempParser <*> minTempParser <*> seedParser

main :: IO ()
main = execParser opts >>= run
    where
        opts = info (optionsParser <**>  helper) ( 
            fullDesc <>
            progDesc "Solve Travelling Salesmen Problems (TSP) with Simulated Annealing" <>
            header "SimulatedAnnealing" )

printProgress :: Int -> CostFunc -> Options -> Solution -> IO ()
printProgress dim costF opts (t,c) = do
    putStrLn "Running.."
    let
        tourOk = isTourOk dim (t,c)
        costOk = isCostOk costF (t,c)
        failTxt = "Error: Solution is not valid: " ++
            if (not costOk) then "Final cost '"++ show c ++"' does not match actual '"++  show (tourCostBy costF t) ++"'." 
            else if (not tourOk) then "Tour is not valid." else ""
        resTxt c' n' = "Cost: "++ show c' ++" found in under "++ show n' ++" epochs. Here is the tour.."
        outTxt = if tourOk && costOk then resTxt c dim else failTxt
    print $ outTxt
    print $ toList t

run :: Options -> IO ()
run opts = do
    contents <- readFile $ filePath opts
    let (dim, costF) = readTSP contents
    -- Run solver
    printProgress dim costF opts $ solveTSP dim costF (seed opts) (maxEpochs opts) (minTemp opts) (maxTemp opts)

