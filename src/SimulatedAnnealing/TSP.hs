{- |Module      :  SimulatedAnnealing.TSP
 - 
    The module provides methods used for Simulated Annealing,
    specific to Travelling Salesman Problems.
-}
module SimulatedAnnealing.TSP where

import System.Random ( StdGen, uniformR, mkStdGen )
import System.Random.Shuffle
import Control.Monad.State ( MonadState(put, get), evalState )
import Data.Foldable ( Foldable(toList, foldr') )
import qualified Data.List as L ( nub )
import qualified Data.Sequence as S ( Seq, index, update, fromList, unstableSort, empty )
import SimulatedAnnealing
import GTSP

-- |TSP Solution is a Hamiltonian Tour
type Tour = S.Seq Node

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

sortTour :: Tour -> Tour
sortTour = S.unstableSort

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

moveCostDelta :: Tour -> Move -> Int -> CostFunc -> Cost
moveCostDelta t (i, j) sz costOf =
    let
        costIx x y = costOf (t !? x, t !? y)
        sumCost = sum . map (\[p,q] -> costIx p q)
        (a:(b:(c:_))) = rotate (i-1) $ sz
        (d:(e:(f:_))) = rotate (j-1) $ sz
        edges = L.nub [[a,b], [b,c], [d,e], [e,f]]
        edges' = L.nub $ map (map (swap i j)) edges
    in sumCost edges' - sumCost edges

nextSolTSP :: (GTSP tsp) => tsp -> (Solution Tour -> AState (Solution Tour))
nextSolTSP tsp sol = do
    -- Get the indices of a random four node sub path of the tour
    (i, j) <- randomMove (size tsp)
    -- Calculate energy change
    let delta = moveCostDelta (value sol) (i, j) (size tsp) (costFunc tsp)
        nextTour = update (value sol) (i, j)
    -- Return the new Solution
    return $ Solution nextTour (energy sol + delta)

costBy :: (Foldable t) => CostFunc -> t Node -> Cost
costBy costOf ns = fst $ foldr' f (0, h) ns where
    f curr (acc, prev) = (acc + costOf (curr, prev), curr)
    -- Need h because a tour "loops" back to start)
    h = head $ toList ns

emptyTour :: Tour
emptyTour = S.empty

toTour :: [Node] -> Tour
toTour = S.fromList

fromTour :: Tour -> [Node]
fromTour = toList

trivialTour :: GTSP tsp => tsp -> Tour
trivialTour tsp = toTour (nodes tsp)

randomTour :: GTSP tsp => tsp -> StdGen -> Tour
randomTour tsp g =
    let t = nodes tsp
    in toTour $ shuffle' t (length t) g
    
initialTour :: GTSP tsp => Params Tour -> tsp -> Tour
initialTour params tsp = 
    let tt = trivialTour tsp
        rt = randomTour tsp (mkStdGen $ initSeed params)
    in if randomiseInitialTour params then rt else tt

tourCost :: (GTSP a) => a -> Tour -> Cost
tourCost tsp = costBy (costFunc tsp)

tourToSolution :: GTSP a => a -> Tour -> Solution Tour
tourToSolution tsp t = Solution t (tourCost tsp t)

initStateTSP :: GTSP tsp => Params Tour -> tsp -> Epoch Tour
initStateTSP params tsp = initState (tempAt params) (tourToSolution tsp $ initialTour params tsp)

minimiseTSP :: GTSP tsp => Params Tour -> tsp -> [Epoch Tour]
minimiseTSP params tsp = evalMinimise params (initStateTSP params tsp)
