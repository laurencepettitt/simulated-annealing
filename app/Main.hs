{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Arrow
import Options.Applicative
import Data.Foldable ( toList )
import qualified SimulatedAnnealing as SA
import SimulatedAnnealing.TSP
import TSPLIB95 ( showError, readEitherTSP )
import GTSP

data Options = Options {
    filePath :: FilePath,
    maxEpochs :: Int,
    maxTemp :: Double,
    minTemp :: Double,
    seed :: Int
} deriving Show

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

optionsParser :: Parser Options
optionsParser = Options <$> filePathParser <*> maxEpochsParser <*> maxTempParser <*> minTempParser <*> seedParser

main :: IO ()
main = execParser opts >>= run
    where
        opts = info (optionsParser <**>  helper) (
            fullDesc <>
            progDesc "Solve Travelling Salesmen Problems (TSP) with Simulated Annealing" <>
            header "SimulatedAnnealing" )

optsToParams opts tsp = SA.Params {
    SA.seed = seed opts,
    SA.tempMax = maxTemp opts,
    SA.tempMin = minTemp opts ,
    SA.timeMax = fromIntegral $ maxEpochs opts,
    SA.tempAt = SA.linearTemp (maxTemp opts) (minTemp opts) (fromIntegral $ maxEpochs opts),
    SA.nextSolTo = nextSolTSP tsp
}

solve opts tsp = solveTSP (optsToParams opts tsp) tsp

run :: Options -> IO ()
run opts = do
    putStrLn "Running.."
    tsp <- readEitherTSP (filePath opts)
    let history = right (solve opts) tsp
    case history of
        Left err -> putStrLn $ showError err
        Right sol -> do
            print $ (SA.sol . head) sol
            putStrLn $ "Steps: " ++ show (length sol)
    -- either (putStrLn . showError) (showSol) history
