{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SimulatedAnnealing.TSP where

import SimulatedAnnealing
import Control.Monad.State
import qualified Data.Sequence as S (Seq, index, update, fromList, unstableSort)
import qualified Data.List as L (nub, sort)
-- import Data.Function 
-- import System.Random.Shuffle (shuffle')
import Data.Foldable
-- import Data.Semigroup
import Data.Monoid
import Data.Maybe
import System.Random
import Debug.Trace

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
ix t i = i `mod` (length t)

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
sort t = S.unstableSort t

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
    return $ (i, i+o)

uniformProb :: AState Double
uniformProb = do
            g <- get
            let (x, g') = uniformR (0, 1) g
            put g'
            return x

swap :: (Eq a) => a -> a -> a -> a
swap x y a = if x == a then y else if y == a then x else a

moveEnergy :: Tour -> Move -> TSP -> Cost
moveEnergy t (i, j) TSP{..} =
    let
        ec x y = costOf (t !? x, t !? y)
        -- Cost of a subsequence of the tour (list of indices)
        -- tc = tourCostBy (uncurry ec)
        cc l = let [new,old] = map (sum . (map (uncurry ec))) l in new - old
        (a:(b:(c:_))) = rotateMod (i-1) n
        (d:(e:(f:_))) = rotateMod (j-1) n
        xxx = [a,b,c,d,e,f]
        es = [[a,b], [b,c], [d,e], [e,f]]
        es' = L.nub es
        rs = map (map (swap i j)) es
        rs' = L.nub rs

        ww = sum . (map (\[p,q] -> ec p q))

        r = trace (show (i,j) ++ "\n" ++ show xxx ++ "\n" ++ show es ++ "\n" ++ show es' ++ "\n\n" ++ show rs ++ "\n" ++ show rs' ++ "\n\n\n") ww rs' - ww es'
        -- us = L.nub [i1,i2,i3,j1,j2,j3]

        -- r = case us of 
        --     [_,_,_]         -> trace (show 0) 0
        --     [a,b,c,d]       -> let xxx= cc [[(a,c), (c,b), (b,d)],                  [(a,b), (b,c), (c,d)]] in trace (show xxx) xxx
        --     [a,b,c,d,e]     -> let xxx= cc [[(a,d), (d,c), (c,b), (b,e)],           [(a,b), (b,c), (c,d), (d,e)]]in trace (show xxx) xxx
        --     [a,b,c,d,e,f]   -> let xxx= cc [[(a,e), (e,c), (c,d), (d,b), (b,f)],    [(a,b), (b,c), (c,d), (d,e), (e,f)]]in trace (show xxx) xxx
        --     _               -> error "This should not happen."
        -- r = case L.nub [i1,i2,i3,j1,j2,j3] of 
        --     [_,_,_]         -> 0
        --     [a,b,c,d]       -> tc [a,c,b,d] - tc [a,b,c,d]
        --     [a,b,c,d,e]     -> tc [a,d,c,b,e] - tc [a,b,c,d,e]
        --     [a,b,c,d,e,f]   -> tc [a,e,c,d,b,f] - (tc [a,b,c,d,e,f])
        --     _               -> error "This should not happen."
    in trace ( show r ) r

moveTSP :: TSP -> (Solution -> Temp -> AState Solution)
moveTSP tsp@(TSP{..}) = move where
    move (tour, energy) temp = do
        -- Get the indices of a random four node sub path of the tour
        (i, j) <- randomMove n
        -- Calculate energy change (requires tsp)
        let delta = moveEnergy tour (i, j) tsp
        -- We must decide whether to accept the new tour
        randProb <- uniformProb
        let
            deltaNum = fromIntegral $ delta
            dT = deltaNum / temp
            acceptProb = min 1 $ exp (0 - dT)
            accept = (delta < 0) || (acceptProb > randProb)
        return $ if accept
            then let ttt = (update tour (i, j), energy + delta) in trace (show ttt ++ "accepted") ttt
            else let ttt = (tour, energy) in trace (show ttt ++ "rejected") ttt

tourCostBy :: (Foldable t) => CostFunc -> t Node -> Cost
tourCostBy costOf t = fst $ foldr' f (0, head) t where
    -- t = (toList tour)
--  where
    -- Add the cost of the edge, and pass the node to the next iteration
    f curr (acc, prev) = (acc + costOf (curr,prev), curr)
    -- Needed to make the tour "loops" back to start)
    -- Equivalent to: h == head $ toList tour
    head = fromJust $ getFirst $ foldMap (First . Just) t

toTour :: [Node] -> Tour
toTour nodes = S.fromList nodes

fromTour :: Tour -> [Node]
fromTour tour = toList tour

toSolution :: [Node] -> CostFunc -> Solution
toSolution nodes costOf = (t, tourCostBy costOf t) where t = toTour nodes

trivialSolution :: Int -> CostFunc -> Solution
trivialSolution n costOf = toSolution [0..n-1] costOf

isCostOk :: TSP -> Solution -> Bool
isCostOk TSP{..} (t,c) = 0 == (abs $ c - (tourCostBy costOf t))  -- TODO: Better

isTourOk :: TSP -> Solution -> Bool
isTourOk TSP{..} (t,c) = [0..n-1] == (fromTour $ sort t)

isSolutionOk :: TSP -> Solution -> Bool
isSolutionOk tsp s = isTourOk tsp s && isCostOk tsp s

-- randomSolution :: StdGen -> Int -> CostFunc -> [Int]
-- randomSolution g numNodes edgeCost = (tour (shuffle' nodes numNodes g), tourCostBy edgeCost nodes) where
    -- nodes = [0..numNodes]
