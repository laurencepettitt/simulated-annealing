{- |Module      :  SimulatedAnnealing

    The module provides high-level methods for optimisation with simulated annealing.
-}
module SimulatedAnnealing where

import System.Random ( StdGen, UniformRange, uniformR, mkStdGen)
import Control.Monad 
import Control.Monad.State
    ( State, MonadState(put, get), evalState )
import Data.Foldable ( foldlM, maximumBy )
import Data.Ord ( comparing )

type Energy = Double
type Temp = Double
type Time = Double

type AState = State StdGen

data Solution s = Solution {
    value :: s,
    energy :: Energy
}

instance (Show s) => Show (Solution s) where
    show sol =  "Value: " ++ show (value sol) ++ "\n" ++
                "Energy: " ++ show (energy sol)

data SimState s = SimState {
    time :: Time,
    temp :: Temp,
    sol :: Solution s,
    solBest :: Solution s,
    isNewSol :: Bool
}

data Params s = Params {
    seed :: Int,
    tempMax :: Temp,
    tempMin :: Temp,
    timeMax :: Time,
    tempAt :: Time -> Temp,
    nextSolTo :: Solution s -> AState (Solution s)
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
stateAccepts :: SimState s -> Solution s -> AState Bool
stateAccepts st newSol = do
    prob <- uniformProb
    let eDelta = energy newSol - energy (sol st)
    return $ (eDelta < 0) || prob < min 1 (exp (negate eDelta / temp st))

move :: (Solution s -> AState (Solution s)) -> SimState s -> AState (SimState s)
move nextSolTo st = do
    nextSol <- nextSolTo (sol st)
    accepts <- stateAccepts st nextSol
    return $ st {
        sol = if accepts then nextSol else sol st,
        solBest = maximumBy (comparing energy) [nextSol, solBest st],
        isNewSol = accepts
    }

tick :: Params s -> SimState s -> AState (SimState s)
tick p st = do
    return $ st {
        time = time st + 1,
        temp = tempAt p (time st + 1)
    }

epoch :: Params s -> SimState s -> AState (SimState s)
epoch  p st = move (nextSolTo p) st >>= tick p

inBounds :: Params s -> SimState s -> Bool
inBounds p st = temp st >= tempMin p && temp st <= tempMax p && time st <= timeMax p

epoch' :: Params s -> [SimState s] -> AState [SimState s]
epoch' p (st : rs) = epoch p st >>= (\st' -> return (st': st : rs))
epoch' p [] = return []

epoch'' :: Params s -> AState (SimState s) -> AState (SimState s)
epoch'' p a = a >>= (move (nextSolTo p) >=> tick p)

-- TODO: stop when temp reaches minTemp
simState :: Params s -> SimState s -> AState ([SimState s])
simState p initial = foldM (\st t -> epoch' p st) [initial] $ takeWhile (\time -> tempAt p time >= tempMin p) [0 .. timeMax p - 1]

-- simState' :: Params s -> AState (SimState s) -> AState [SimState s]
--simState' p ini = do
    --let
        --hist = (:) <$> ini <*> hist'
        --hist' = 


--simState :: Params s -> SimState s -> AState [SimState s]
--simState p ini = do simState' p (return ini)

runSim p initial = evalState (simState p initial) (mkStdGen $ seed p)

-- simState :: Params s -> SimState s -> SimState s
-- simState p st = evalState (epoch p st) (gen st)

-- runSim :: Params s -> SimState s -> [SimState s]
-- runSim p initial = takeWhile (inBounds p) sims where
--     sims = initial : map (simState p) sims

initSim :: (Time -> Temp) -> Solution s -> SimState s
initSim tempAt initialSol = SimState {
    time = 0,
    temp = tempAt 0,
    sol = initialSol,
    solBest = initialSol,
    isNewSol = False
}

----------------------------------------------------------------------------
-- Temperature Schedules

-- additiveTempSched :: (Enum t, RealFloat t) => (t -> t -> t -> t -> t) -> (t -> t -> Int -> [t])
-- additiveTempSched tempAt hi lo n = takeWhile (>0) $ map (tempAt hi lo n) [0..n]

linearTemp hi lo n k = lo + (hi - lo) * ((n - k) / n)

-- linearAdditiveTempSched = additiveTempSched linearTemp

exponentialTemp hi lo n k =
    let r = hi - lo
    in lo + r * (1 / (1 + exp (log 2 * r / n) * (k - n / 2)))

-- exponentialAdditiveTempSched = additiveTempSched exponentialTemp
