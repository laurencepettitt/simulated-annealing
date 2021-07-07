module Main where

import Test.Hspec
import SimulatedAnnealing
import SimulatedAnnealing.TSP
import TSPLIB95
import GTSP
import Debug.Trace

-- optSolution tsp opt = 
    -- let TourSection tours = (opt ! kTOUR_SECTION)
        -- tour = head tours
    -- in (tour, tourCostBy (costOf tsp) tour)

--tspSpec :: String -> IO (Spec)
--tspSpec name = do
    --let
        --tspPath = (basePath ++ name ++ ".tsp")
        --optTourPath = (basePath ++ name ++ ".opt.tour")
        --params tsp = mkParams 42 5 0 700000 linearTemp (nextSolTSP  tsp)
    --Just tsp <- readMaybeTSP tspPath
    --Just optTour <- readMaybeTSP optTourPath
    --let sol@(t, c) = minimiseTSP (params tsp) tsp
    --let sol'@(t', c') = optSolution tsp optTour
    --print sol
    --putStrLn ""
    --print sol'
    --return $ context name $ do
        --context "good solution" $ do
            --it "has valid tour (visits all nodes)" $ do
                 --nodes tsp == fromTour (sortTour t)
            --it "is consistent (cost really is the cost of the tour)" $ do 
                --let c' = tourCostBy (costOf tsp) t
                --(abs (c' - c) / max c' c) < 0.000001
            --it "is not trivial solution (e.g. [1,2,3,..,n])" $ do
                --t /= sortTour t
            --it "is not worse than trivial solution" $ do
                --c <= snd (trivialTour tsp)
            --it "is not better than optimal" $ do
                --c >= c'
            --it "is no more than twice optimal" $ do
                --c / c' < 2

main :: IO ()
main = do
    return ()
    --a280Spec <- tspSpec "a280"
    --hspec $ do
        --context "solve TSPLIB95" $ do
            --a280Spec
