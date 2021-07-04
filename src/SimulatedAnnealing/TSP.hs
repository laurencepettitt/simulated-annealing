{- |Module      :  SimulatedAnnealing.TSP
 - 
    The module provides methods used for Simulated Annealing,
    specific to Travelling Salesman Problems.
-}
module SimulatedAnnealing.TSP where

import System.Random ( uniformR, mkStdGen )
import Control.Monad.State ( MonadState(put, get), evalState )
import Data.Foldable ( Foldable(toList, foldr') )
import qualified Data.List as L ( nub )
import qualified Data.Sequence as S ( Seq, index, update, fromList, unstableSort )
import SimulatedAnnealing
import GTSP

-- |TSP Solution is a Hamiltonian Tour
type Tour = S.Seq Node

-- showTour :: Solution -> String
-- showTour (t, c) = "Cost: "++ show c ++"\nTour: "++ show (toList t)

-- |A move from one solution to a neighbouring solution is pair of indicies
-- representing a swap between the corresponding nodes in the tour
type Move = (Int, Int)

-- |rotate s m returns a cycle, modulo m, starting at s.
--          Like so :    s, s+1, s+2, ..., m-2, m-1, 0, 1, 2, ..., etc
rotate :: (Integral a) => a -> a -> [a]
rotate s m = [(i + s) `mod` m | i <- [0..]]

-- |ix t i returns i modulo length of t.
-- i.e the index in t that corresponds to the offset i in a cycle of size t.
ix :: (Foldable t, Integral i) => t n -> i -> i
ix t i = fromIntegral i `mod` fromIntegral (length t)

-- | t !? i gets the node and index i in tour t
(!?) :: Tour -> Int -> Node
(!?) t i = S.index t $ ix t i

sortSolution :: Tour -> Tour
sortSolution = S.unstableSort

update :: Tour -> Move -> Tour
update t (i, j) =
    let
        (i', j') = (ix t i, ix t j)
        (u, v) = (t !? i', t !? j')
    in
        S.update j' u $ S.update i' v t

-- |randomMove n chooses two distinct random positions in
-- the range [0..n), representing a pair of nodes in a
-- Hamiltonian Tour to  potentially swap places.
randomMove :: Int -> AState Move
randomMove n = do
    i <- uniformPos 0 (n-1)
    o <- uniformPos 1 (n-i)
    return (i, i+o)

swap :: (Eq a) => a -> a -> a -> a
swap x y a
        | x == a = y
        | y == a = x
        | otherwise = a

moveEnergyDelta :: Tour -> Move -> Int -> CostFunc -> Cost
moveEnergyDelta t (i, j) sz cost =
    let
        costIx x y = cost (t !? x, t !? y)
        sumCost = sum . map (\[p,q] -> costIx p q)
        (a:(b:(c:_))) = rotate (i-1) $ sz
        (d:(e:(f:_))) = rotate (j-1) $ sz
        edges = L.nub [[a,b], [b,c], [d,e], [e,f]]
        edges' = L.nub $ map (map (swap i j)) edges
    in sumCost edges' - sumCost edges

-- type MoveFunc = Int -> CostFunc -> (SimState -> AState SimState)

nextSolTSP :: (GTSP tsp) => tsp -> (Solution Tour -> AState (Solution Tour))
nextSolTSP tsp sol = do
    -- Get the indices of a random four node sub path of the tour
    (i, j) <- randomMove (size tsp)
    -- Calculate energy change
    let delta = moveEnergyDelta (value sol) (i, j) (size tsp) (costFunc tsp)
        nextTour = update (value sol) (i, j)
    -- Return the new Solution
    return $ Solution nextTour (energy sol + delta)

energyBy :: (Foldable t) => CostFunc -> t Node -> Cost
energyBy cost ns = fst $ foldr' f (0, h) ns where
    f curr (acc, prev) = (acc + cost (curr, prev), curr)
    -- Need h because a tour "loops" back to start)
    h = head $ toList ns

toTour :: [Node] -> Tour
toTour = S.fromList

fromTour :: Tour -> [Node]
fromTour = toList

trivialTour :: GTSP tsp => tsp -> Tour
trivialTour tsp = toTour (nodes tsp)

tourEnergy :: (GTSP a) => a -> Tour -> Cost
tourEnergy tsp = energyBy (costFunc tsp)

-- randomSolution :: StdGen -> Int -> CostFunc -> [Int]
-- randomSolution g numNodes edgeCost = (tour (shuffle' nodes numNodes g), tourCostBy edgeCost nodes) where
    -- nodes = [0..numNodes]

tourToSolution :: GTSP a => a -> Tour -> Solution Tour
tourToSolution tsp t = Solution t (tourEnergy tsp t)

solveTSP :: GTSP tsp => Params Tour -> tsp -> [SimState Tour]
solveTSP params tsp = runSim params (initSim (tempAt params) (tourToSolution tsp (trivialTour tsp)))
