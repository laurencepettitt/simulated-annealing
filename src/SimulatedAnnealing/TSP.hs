{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SimulatedAnnealing.TSP where

import SimulatedAnnealing ( AState )
import Control.Monad.State ( MonadState(put, get) )
import qualified Data.Sequence as S (Seq, index, update, fromList, unstableSort)
import qualified Data.List as L (nub, sort)
-- import Data.Function 
-- import System.Random.Shuffle (shuffle')
import Data.Foldable ( Foldable(toList, foldr') )
-- import Data.Semigroup
import Data.Monoid ( First(First, getFirst) )
import Data.Maybe ( fromJust )
import System.Random ( uniformR )
import Debug.Trace ()

type Temp = Double
type Cost = Integer
type Node = Int

type CostFunc = (Node, Node) -> Cost

-- Travelling Sales Problem
data TSP = TSP {
    -- Number of nodes (dimension of TSP)
    n :: Int,
    -- Cost function (edge -> weight)
    costOf :: CostFunc
}

-- TSP Solution is a Hamiltonian Tour and the Cost (weight/energy) of that tour
type Tour = S.Seq Node
type Solution = (Tour, Cost)

-- A move from one solution to a neighbouring solution is a swap between two nodes in the tour
type Move = (Node, Node)

ix :: Tour -> Int -> Int
ix t i = i `mod` length t

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

uniformMod' :: Int -> Int -> AState Int
uniformMod' lo mod = do
    g <- get
    let (x, g') = uniformR (lo, mod-1) g
    put g'
    return x

rotateMod :: (Integral a) => a -> a -> [a]
rotateMod s m = [(i + s) `mod` m | i <- [0..]]

randomMove :: Int -> AState Move
randomMove n = do
    i <- uniformMod' 0 (n-1)
    o <- uniformMod' 1 (n-i)
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

moveEnergy :: Tour -> Move -> TSP -> Cost
moveEnergy t (i, j) TSP{..} =
    let
        costI x y = costOf (t !? x, t !? y)
        sumCost = sum . map (\[p,q] -> costI p q)
        (a:(b:(c:_))) = rotateMod (i-1) n
        (d:(e:(f:_))) = rotateMod (j-1) n
        edges = L.nub [[a,b], [b,c], [d,e], [e,f]]
        edges' = L.nub $ map (map (swap i j)) edges
    in sumCost edges' - sumCost edges

moveTSP :: TSP -> (Solution -> Temp -> AState Solution)
moveTSP tsp@TSP{..} = move where
    move (tour, energy) temp = do
        -- Get the indices of a random four node sub path of the tour
        (i, j) <- randomMove n
        -- Calculate energy change (requires tsp)
        let delta = moveEnergy tour (i, j) tsp
        -- We must decide whether to accept the new tour
        randProb <- uniformProb
        let
            deltaNum = fromIntegral delta
            acceptWorse = randProb < min 1 (exp (negate deltaNum / temp))
            accept = (delta < 0) || acceptWorse
            tour' = update tour (i, j)
            energy' = energy + delta
        return $ if accept then (tour', energy') else (tour, energy)

tourCostBy :: (Foldable t) => CostFunc -> t Node -> Cost
tourCostBy costOf t = fst $ foldr' f (0, head) t where
    f curr (acc, prev) = (acc + costOf (curr,prev), curr)
    -- Need head to make sure the tour "loops" back to start)
    head = fromJust $ getFirst $ foldMap (First . Just) t

toTour :: [Node] -> Tour
toTour = S.fromList

fromTour :: Tour -> [Node]
fromTour = toList

toSolution :: [Node] -> CostFunc -> Solution
toSolution nodes costOf = (t, tourCostBy costOf t) where t = toTour nodes

trivialSolution :: Int -> CostFunc -> Solution
trivialSolution n = toSolution [0..n-1]

isCostOk :: TSP -> Solution -> Bool
isCostOk TSP{..} (t,c) = 0 == abs  c - tourCostBy costOf t  -- TODO: Better

isTourOk :: TSP -> Solution -> Bool
isTourOk TSP{..} (t,c) = [0..n-1] == fromTour (sort t)

isSolutionOk :: TSP -> Solution -> Bool
isSolutionOk tsp s = isTourOk tsp s && isCostOk tsp s

-- randomSolution :: StdGen -> Int -> CostFunc -> [Int]
-- randomSolution g numNodes edgeCost = (tour (shuffle' nodes numNodes g), tourCostBy edgeCost nodes) where
    -- nodes = [0..numNodes]
