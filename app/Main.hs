{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import SimulatedAnnealing
import SimulatedAnnealing.TSP
import SimulatedAnnealing.TSPLIB95
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

printProgress :: TSP -> Options -> Solution -> IO ()
printProgress tsp@(TSP{..}) Options{..} (t,c) = do
    putStrLn "Running.."
    let
        tourOk = isTourOk tsp (t,c)
        costOk = isCostOk tsp (t,c)
        failTxt = "Error: Solution is not valid: " ++
            if (not costOk) then "Final cost '"++ show c ++"' does not match actual '"++  show (tourCostBy costOf t) ++"'." 
            else if (not tourOk) then "Tour is not valid." else ""
        resTxt c' n' = "Cost: "++ show c' ++" found in under "++ show n' ++" epochs. Here is the tour.."
        outTxt = if isSolutionOk tsp (t,c) then resTxt c n else failTxt
    print $ outTxt
    print $ toList t

run :: Options -> IO ()
run opts = do
    contents <- readFile $ filePath opts
    let tsp = readTSP contents
    -- Run solver
    printProgress tsp opts $ solveTSP tsp opts

solveTSP :: TSP -> Options -> Solution
solveTSP tsp@(TSP{..}) Options{..} =
    let
        -- Setup annealing schedule
        temps = linearTempSched maxEpochs maxTemp minTemp
        -- Create initial solution
        init = trivialSolution n costOf
        -- Create PRNG
        gen = (mkStdGen seed)
        -- Create state
        solveState = solve (moveTSP tsp) init temps
        -- Run solver
    in trace ("init: " ++ show init) $ evalState solveState gen



-- -- Main
-- main :: IO ()
-- main = do
--     args0 <- getArgs
--     let

--         filePath :: String
--         maxTemp :: Double
--         maxSteps :: Double
--         (filePathM, args1) = popM args0
--         (maxTempM, args2) = popM args1
--         (maxStepsM, args3) = popM args2
--     contents <- readFile filePath
--     let
--         -- Read coords from file into dictionary
--         s = sections contents
--         numNodes = read $ s M.! "DIMENSION"
--         nodeCoordsRaw = s M.! "NODE_COORD_SECTION"
--         ws = words nodeCoordsRaw
--         is = map (read) ws :: [Int]
--         ts = triples is
--         coords = M.fromList $ zip [0..] $ map (\(x,y,z) -> (y,z)) ts
--         -- Create TSP
--         tsp = TSP { numNodes = numNodes, edgeWeight = weight coords }
--         -- Setup solve parameters
--         temps = takeWhile (>0) $ map (\x -> maxTemp * (1 - (x / maxSteps)) ) [0..maxSteps]
--         numSteps = length temps
--         initialTour = shuffle' [0..numNodes-1] numNodes (mkStdGen 5023423098765)
--         initialEnergy = tourWeightBy (edgeWeight tsp) initialTour
--         initialSolution = (S.fromList initialTour, initialEnergy)
--         -- Run solve
--         (optTour, optWeight) = evalState (solve (moveForTSP tsp) initialSolution temps) (mkStdGen 49)
--         finalTour = toList optTour
--         -- Put together result strings
--         shuffledTourOk = [0..numNodes-1] == (L.sort initialTour)
--         finalTourOk = [0..numNodes-1] == L.sort finalTour
--         optWeightStr = show optWeight
--         maxTempStr = show maxTemp
--         text = ((filter (\c -> c == 's') optWeightStr) ++ "Weight: " ++ optWeightStr ++ ", found in " ++ show numSteps ++ " iterations. Here is the tour..")
--     -- Print results
--     putStrLn $ "Shuffled tour " ++ if shuffledTourOk then "ok." else "not ok."
--     putStrLn $ "Final tour " ++ if finalTourOk then "ok." else "not ok."
--     putStrLn "Running.."
--     putStrLn text
--     print optTour
--     putStrLn "First 50 temps.."
--     print $ take 50 temps
--     putStrLn "Last 50 temps.."
--     print $ drop ((length temps) - 50) temps
--     putStrLn ""
--     trueOptTour <- getTrueOptTour
--     print $ tourWeightBy (edgeWeight tsp) trueOptTour
--     print trueOptTour