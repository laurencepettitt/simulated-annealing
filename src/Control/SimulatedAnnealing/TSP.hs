module Control.SimulatedAnnealing.TSP ( solveTSP, Tour, Solution, isTourOk, isCostOk, tourCostBy ) where

import Data.TSP ( CostFunc, Node, Cost )
import Control.SimulatedAnnealing ( AState, Temp, solve, linearTempSched )
import Control.Monad.State ( MonadState(put, get), evalState )
import qualified Data.Sequence as S (Seq, index, update, fromList, unstableSort)
import qualified Data.List as L (nub)
import Data.Foldable ( Foldable(toList, foldr') )
import System.Random ( uniformR, mkStdGen )

-- TSP Solution is a Hamiltonian Tour and the Cost (weight/energy) of that tour
type Tour = S.Seq Node
type Solution = (Tour, Cost)

-- A move from one solution to a neighbouring solution is pair of indicies
-- representing a swap between the corresponding nodes in the tour
type Move = (Int, Int)

ix :: (Foldable t, Integral i) => t n -> i -> i
ix t i = fromIntegral i `mod` fromIntegral (length t)

(!?) :: Tour -> Int -> Node
(!?) t i = S.index t $ ix t i

update :: Tour -> Move -> Tour
update t (i, j) =
    let
        (i', j') = (ix t i, ix t j)
        (u, v) = (t !? i', t !? j')
    in
        S.update j' u $ S.update i' v t

sort :: Tour -> Tour
sort = S.unstableSort

uniformPos :: Int -> Int -> AState Int
uniformPos lo dim = do
    g <- get
    let (x, g') = uniformR (lo, dim-1) g
    put g'
    return x

rotate :: (Integral a) => a -> a -> [a]
rotate s m = [(i + s) `mod` m | i <- [0..]]

randomMove :: Int -> AState Move
randomMove n = do
    i <- uniformPos 0 (n-1)
    o <- uniformPos 1 (n-i)
    return (i, i+o)

uniformProb :: AState Double
uniformProb = do
            g <- get
            let (x, g') = uniformR (0, 1) g
            put g'
            return x

swap :: (Eq a) => a -> a -> a -> a
swap x y a
  | x == a = y
  | y == a = x
  | otherwise = a

moveEnergy :: Tour -> Move -> Int -> CostFunc -> Cost
moveEnergy t (i, j) dim cost =
    let
        costIx x y = cost (t !? x, t !? y)
        sumCost = sum . map (\[p,q] -> costIx p q)
        (a:(b:(c:_))) = rotate (i-1) $ dim
        (d:(e:(f:_))) = rotate (j-1) $ dim
        edges = L.nub [[a,b], [b,c], [d,e], [e,f]]
        edges' = L.nub $ map (map (swap i j)) edges
    in sumCost edges' - sumCost edges

moveTSP :: Int -> CostFunc -> (Solution -> Temp -> AState Solution)
moveTSP dim cost = move where
    move (tour, energy) temp = do
        -- Get the indices of a random four node sub path of the tour
        (i, j) <- randomMove $ dim
        -- Calculate energy change (requires tsp)
        let delta = moveEnergy tour (i, j) dim cost
        -- We must decide whether to accept the new tour
        randProb <- uniformProb
        let
            acceptWorse = randProb < min 1 (exp (negate delta / temp))
            accept = (delta < 0) || acceptWorse
            tour' = update tour (i, j)
            energy' = energy + delta
        return $ if accept then (tour', energy') else (tour, energy)

tourCostBy :: (Foldable t) => CostFunc -> t Node -> Cost
tourCostBy costOf t = fst $ foldr' f (0, h) t where
    f curr (acc, prev) = (acc + costOf (curr,prev), curr)
    -- Need h because a tour "loops" back to start)
    h = head $ toList t

toTour :: [Node] -> Tour
toTour = S.fromList

fromTour :: Tour -> [Node]
fromTour = toList


toSolution :: [Node] -> CostFunc -> Solution
toSolution nodes cost = (toTour nodes, tourCostBy cost nodes)

trivialSolution :: Int -> CostFunc -> Solution
trivialSolution dim = toSolution [0 .. dim-1]

isCostOk :: CostFunc -> Solution -> Bool
-- Checks that, at the end of the simulation, the final
-- cost is close to the actual cost of the final tour
isCostOk cost (t,c) = let c' = tourCostBy cost t in 0.0001 > abs (c' - c) / max c' c

isTourOk :: Int -> Solution -> Bool
isTourOk dim (t,c) = [0 .. dim-1] == fromTour (sort t) -- TODO: better (tour is general traversible type)

isSolutionOk :: Int -> CostFunc -> Solution -> Bool
isSolutionOk dim cost sol = isTourOk dim sol && isCostOk cost sol

-- randomSolution :: StdGen -> Int -> CostFunc -> [Int]
-- randomSolution g numNodes edgeCost = (tour (shuffle' nodes numNodes g), tourCostBy edgeCost nodes) where
    -- nodes = [0..numNodes]

solveTSP :: Int -> CostFunc -> Int -> Int -> Temp -> Temp -> Solution
solveTSP dim cost seed maxEpochs minTemp maxTemp =
    let
        -- Setup annealing schedule
        temps = linearTempSched maxEpochs maxTemp minTemp
        -- Create initial solution
        init = trivialSolution dim cost
        -- Create PRNG
        gen = mkStdGen seed
        -- Create state
        solveState = solve (moveTSP dim cost) init temps
        -- Run solver
    in evalState solveState gen