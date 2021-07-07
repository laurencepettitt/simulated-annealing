{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{- |Module      :  SimulatedAnnealing

    The module provides high-level methods for optimisation with simulated annealing.
-}
module SimulatedAnnealing where

import GHC.Generics
import System.Random ( StdGen, UniformRange, uniformR, mkStdGen )
import Control.Monad
import Control.Monad.State.Lazy
    ( State, MonadState(put, get), evalState, runState )
import Data.Foldable ( foldlM, maximumBy )
import Data.Aeson hiding (Options)
import Data.Ord ( comparing )

type Energy = Double
type Temp = Double
type Time = Double

type AState = State StdGen

data Solution s = Solution {
    value :: s,
    energy :: Energy
} deriving (Generic, Show)

instance (ToJSON s) => ToJSON (Solution s) where
    toEncoding = genericToEncoding defaultOptions
    
data Epoch s = Epoch {
    time :: Time,
    temp :: Temp,
    sol :: Solution s,
    bestSol :: Solution s,
    isNewSolution :: Bool
} deriving (Generic, Show)
    
instance (ToJSON s) => ToJSON (Epoch s) where
    toEncoding = genericToEncoding defaultOptions

data Params s = Params {
    initSeed :: Int,
    tempMax :: Temp,
    tempMin :: Temp,
    timeMax :: Time,
    tempAt :: Time -> Temp,
    nextSolTo :: Solution s -> AState (Solution s)
}

mkParams seed' tempMax' tempMin' timeMax' tempFunc nextSolFunc = Params {
        initSeed = seed',
        tempMax = tempMax',
        tempMin = tempMin',
        timeMax = timeMax',
        tempAt = linearTemp tempMax' tempMin' timeMax',
        nextSolTo = nextSolFunc
    }

-- |chooses a random probability.
uniformProb :: (RealFloat a, UniformRange  a) => AState a
uniformProb = do
            g <- get
            let (x, g') = uniformR (0, 1) g
            put g'
            return x

-- |uniformPos lo dim chooses a random position,
-- with uniform distribution, in the range [lo, dim)
uniformPos :: (Integral a, UniformRange a) => a -> a -> AState a
uniformPos lo dim = do
    g <- get
    let (x, g') = uniformR (lo, dim-1) g
    put g'
    return x

-- |move sol sol' accepts sol' to replace sol if it is better or
--  even if it is worse but with a probability depending on temp
stateAccepts :: Epoch s -> Solution s -> AState Bool
stateAccepts st newSol = do
    prob <- uniformProb
    let eDelta = energy newSol - energy (sol st)
    return $ (eDelta < 0) || prob < min 1 (exp (negate eDelta / temp st))

move :: (Solution s -> AState (Solution s)) -> Epoch s -> AState (Epoch s)
move nextSolTo st = do
    nextSol <- nextSolTo (sol st)
    accepts <- stateAccepts st nextSol
    return $ st {
        sol = if accepts then nextSol else sol st,
        bestSol = maximumBy (comparing energy) [nextSol, bestSol st],
        isNewSolution = accepts
    }

tick :: Params s -> Epoch s -> AState (Epoch s)
tick p st = do
    return $ st {
        time = time st + 1,
        temp = tempAt p (time st + 1)
    }

epoch :: Params s -> Epoch s -> AState (Epoch s)
epoch  p st = move (nextSolTo p) st >>= tick p

epoch' :: Params s -> [Epoch s] -> AState [Epoch s]
epoch' p (st : rs) = epoch p st >>= (\st' -> return (st': st : rs))
epoch' p [] = return []

-- epoch'' :: Params s -> AState (Epoch s) -> AState (Epoch s)
-- epoch'' p a = a >>= (move (nextSolTo p) >=> tick p)

inBounds :: Params s -> Epoch s -> Bool
inBounds p st = temp st >= tempMin p && temp st <= tempMax p && time st <= timeMax p

-- TODO: stop when temp reaches minTemp
minimiseM :: Params s -> Epoch s -> AState [Epoch s]
minimiseM p initial = foldM (\st t -> epoch' p st) [initial] $ takeWhile (\time -> tempAt p time >= tempMin p) [0 .. timeMax p - 1]

runMinimise :: Params s -> Epoch s -> StdGen -> ([Epoch s], StdGen)
runMinimise p initial gen = runState (minimiseM p initial) gen

evalMinimise :: Params s -> Epoch s -> [Epoch s]
evalMinimise p initial = evalState (minimiseM p initial) (mkStdGen $ initSeed p)

initState :: (Time -> Temp) -> Solution s -> Epoch s
initState tempAt initialSol = Epoch {
    time = 0,
    temp = tempAt 0,
    sol = initialSol,
    bestSol = initialSol,
    isNewSolution = False
}

flattenTrials :: [[s]] -> [s]
flattenTrials runs = [ ep | run <- runs, ep <- run]

acceptanceRate :: [[Epoch s]] -> Double
acceptanceRate r =
    let rs = flattenTrials r
        numAccepted = fromIntegral . sum . map (fromEnum . isNewSolution)
        total = fromIntegral . length
    in numAccepted rs / total rs

----------------------------------------------------------------------------
-- Temperature Schedules


linearTemp hi lo n k = lo + (hi - lo) * ((n - k) / n)

exponentialTemp hi lo n k =
    let r = hi - lo
    in lo + r * (1 / (1 + exp (log 2 * r / n) * (k - n / 2)))
