{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import System.Random
import Control.Arrow
import Options.Applicative
import Control.Monad.State.Lazy
import Data.Foldable ( toList )
import Data.Traversable (traverse)
import Data.Aeson hiding (Options)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Types (listEncoding)
import qualified Data.ByteString.Lazy as B
import qualified SimulatedAnnealing as SA
import SimulatedAnnealing.TSP
import TSPLIB95 ( TSP, showError, readEitherTSP )
import GTSP

data Options = Options {
    filePath :: FilePath,
    maxEpochs :: Int,
    maxTemp :: Double,
    minTemp :: Double,
    seed :: Int,
    rndInitTour :: Bool
} deriving Show

data ExpOptions = ExpOptions {
    iterations :: Int,
    expOptions :: Options
}

data SolveOptions = SolveOptions {
    slvOptions :: Options
}

data Cmd = Experiment ExpOptions | Solve SolveOptions

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

rndInitTourParser :: Parser Bool
rndInitTourParser = switch (
    long "random_initial_tour" <>
    help "Randomly shuffles the initial tour, using seed")

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

iterationsParser :: Parser Int
iterationsParser = option auto (
    long "trials" <>
    metavar "NUM" <>
    value 5 <>
    help "Number of times to run experiment")

optionsParser :: Parser Options
optionsParser = Options <$> filePathParser <*> maxEpochsParser <*> maxTempParser <*> minTempParser <*> seedParser <*> rndInitTourParser

experimentOptions :: Parser Cmd
experimentOptions = Experiment <$> (ExpOptions <$> iterationsParser <*> optionsParser)

solveOptions :: Parser Cmd
solveOptions = Solve <$> (SolveOptions <$> optionsParser)

cmdParser :: Parser Cmd
cmdParser = subparser (
    command "experiment" (info experimentOptions (progDesc "Run experiments (multiple simulations)")) <>
    command "solve" (info solveOptions (progDesc "Solve a TSP (one simulation)")))

optsToParams :: Options -> TSP -> SA.Params Tour
optsToParams opts tsp = SA.Params {
    SA.initSeed = seed opts,
    SA.randomiseInitialTour = rndInitTour opts,
    SA.tempMax = maxTemp opts,
    SA.tempMin = minTemp opts,
    SA.timeMax = fromIntegral $ maxEpochs opts,
    SA.tempAt = SA.linearTemp (maxTemp opts) (minTemp opts) (fromIntegral $ maxEpochs opts),
    SA.nextSolTo = nextSolTSP tsp
}

experiment :: Int -> (SA.Params Tour) -> TSP -> [[SA.Epoch Tour]]
experiment iters params tsp =
    let
        initialState = initStateTSP params tsp
        initialGen = mkStdGen $ SA.initSeed params
        exp = replicateM iters $ SA.minimiseM params initialState
    in
        evalState exp initialGen

solve :: (SA.Params Tour) -> TSP -> SA.Epoch Tour
solve params tsp =
    let
        initialState = initStateTSP params tsp
        initialGen = mkStdGen $ SA.initSeed params
    in 
        head $ evalState (SA.minimiseM params initialState) initialGen

epochToEncoding' :: SA.Epoch s -> Encoding
epochToEncoding' (ep) = 
    pairs ("time" .= SA.time ep <>
            "temp" .= SA.temp ep <>
            "energy" .= SA.energy (SA.sol ep) <>
            "bestEnergy" .= SA.energy (SA.bestSol ep) <>
            "isNewSolution" .= SA.isNewSolution ep)

execReadTSP :: Options -> (TSP -> IO ()) -> IO ()
execReadTSP opts handle = do
    tspRead <- readEitherTSP (filePath opts)
    case tspRead of
        Left err -> putStrLn $ showError err
        Right tsp -> handle tsp

execExperiment :: Int -> Options -> TSP -> IO ()
execExperiment iters opts tsp =
    let putRes = B.putStr . encodingToLazyByteString . listEncoding epochToEncoding'
        res = SA.flattenTrials $ experiment iters (optsToParams opts tsp) tsp
    in putRes res

execSolve :: Options -> TSP -> IO ()
execSolve opts tsp = B.putStr $ encode $ solve (optsToParams opts tsp) tsp

exec :: Cmd -> IO ()
exec (Experiment (ExpOptions iters opts)) = execReadTSP opts $ execExperiment iters opts
exec (Solve (SolveOptions opts)) = execReadTSP opts $ execSolve opts

main :: IO ()
main = execParser cmd >>= exec
    where
        cmd = info (cmdParser <**> helper) (
            fullDesc <>
            progDesc "Commands to solve Travelling Salesmen Problems (TSP) with Simulated Annealing" <>
            header "SimulatedAnnealing" )
