module Main where

import Test.Hspec
import SimulatedAnnealing
import SimulatedAnnealing.TSP
import TSPLIB95
import GTSP
import Debug.Trace

optimumSolution tsp opt_tsplib95 = 
     let TourSection tours = (opt_tsplib95 ! kTOUR_SECTION)
         tour = head tours
     in Solution {value = tour, energy = costBy (costFunc tsp) tour}

tspSpec :: String -> IO (Spec)
tspSpec name = do
    let tspPath = (basePath ++ name ++ ".tsp")
        optTourPath = (basePath ++ name ++ ".opt.tour")
    Just tsp <- readMaybeTSP tspPath
    Just optTour <- readMaybeTSP optTourPath
    let params = mkParams 43 True 6 0 500000 linearTemp (nextSolTSP  tsp)
        hist = minimiseTSP params tsp
    let res_sol = sol $ head hist
    let opt_sol = optimumSolution tsp optTour
    let val = value res_sol
    let en = energy res_sol
    let en_opt = energy opt_sol
    print (tourToSolution tsp (trivialTour tsp))
    print res_sol
    putStrLn ""
    print opt_sol
    return $ context name $ do
        context "good solution" $ do
            it "has valid tour (visits all nodes)" $ do
                 nodes tsp == fromTour (sortTour val)
            it "is consistent (cost really is the cost of the tour)" $ do
                let en_actual = tourCost tsp val
                (abs (en_actual - en) / max en_actual en) < 0.000001
            it "is not trivial solution (e.g. [1,2,3,..,n])" $ do
                val /= trivialTour tsp
            it "is not more than 2x trivial solution" $ do
                en <= 2 * tourCost tsp (trivialTour tsp)
            it "is not better than optimal" $ do
                en >= en_opt
            it "is not more than 2x optimal" $ do
                en / en_opt < 2

main :: IO ()
main = do
    -- return ()
    a280Spec <- tspSpec "a280"
    hspec $ do
        context "solve TSPLIB95" $ do
            a280Spec
