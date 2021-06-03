module SimulatedAnnealing where
    
import System.Random
import Control.Monad.State
import Data.Foldable (foldlM)

type AState = State StdGen

solve :: (Num temp, Ord temp) => (sol -> temp -> AState sol) -> sol -> [temp] -> AState sol
solve move init sched = do foldlM move init (takeWhile (> 0) sched)

linearTempSched :: Int -> Double -> Double -> [Double]
linearTempSched maxEpochs maxTemp minTemp = 
    let
        maxEpochsD = fromIntegral maxEpochs 
        temp x = minTemp + (maxTemp - minTemp) * ((maxEpochsD - x) / maxEpochsD)
    in 
        takeWhile (>0) $ map temp [0..maxEpochsD]
