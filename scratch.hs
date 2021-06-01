import Data.List

-- type AState = State StdGen
-- solve :: (Num temp, Ord temp) => (solution -> temp -> AState solution) -> solution -> [temp] -> AState solution


-- data StateGenM g
-- Opaque data type that carries the type of a pure pseudo-random number generator.

-- runState :: State s a -> s -> (a, s)
-- Unwrap a state monad computation as a function. 

-- runStateGen :: RandomGen g => g -> (StateGenM g -> State g a) -> (a, g)
-- Runs a monadic generating action in the State monad using a pure pseudo-random number generator.

-- >>> import System.Random.Stateful
-- >>> let pureGen = mkStdGen 137
-- >>> runStateGen pureGen randomM :: (Int, StdGen)
-- (7879794327570578227,StdGen {unStdGen = SMGen 11285859549637045894 7641485672361121627})

-- randomM :: (RandomGenM g r m, Random a) => g -> m a
-- Generates a pseudo-random value using monadic interface and Random instance.



-- randomPhone :: RandomGen g => [AreaCode] -> g -> (Phone, g)
-- randomPhone areaCodes g =
--   let (i, g') = randomR (0, length areaCodes - 1) g
--       (phoneLocalNumber, g'') = randomR (0, 9999999) g'
--    in (Phone {phoneAreaCode = areaCodes !! i, phoneLocalNumber}, g'')