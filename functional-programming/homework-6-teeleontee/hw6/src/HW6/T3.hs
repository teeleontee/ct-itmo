module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid
  , printGrid
  , simulate
  ) where

import Control.Monad

import System.Random
import Data.Grid
import Data.ListZipper
import Control.Comonad (Comonad(duplicate, extend))

-- | data type for configuring wanted properties of our virus i.e.
-- how easily it spreads (probability), the incubationPeriod, illnessDuration
-- and immunityDuration
data Config = Config
  { probability :: Double
  , incubationPeriod :: Int
  , illnessDuration :: Int
  , immunityDuration :: Int
  } deriving Show

-- | A Cell can be Healthy, Infected, Ill or Imune
data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int

instance Show CellState where
  show Healthy      = "_"
  show (Infected _) = "i"
  show (Immune _)   = "@"
  show (Ill _)      = "#"

-- | Cell contains it's state and a generator for creating random numbers
data Cell = Cell
  { cellState :: CellState
  , cellRand :: StdGen
  }

instance Show Cell where
  show = show . cellState 

-- | Type alias for a Grid containing Cells
type Comonad19Grid = Grid Cell

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [left, right]
        verticals   = [up, down]

infectedCount :: [Cell] -> Int
infectedCount = length . filter fn
  where
    fn cell = let s = cellState cell
              in case s of
                Infected _ -> True
                _notInfect -> False

infectedNeighbours :: Comonad19Grid -> Int
infectedNeighbours g = infectedCount $ map (\dir -> extract $ dir g) neighbours

rule :: Config -> Comonad19Grid -> Cell
rule conf g = 
  let cell = extract g
      state = cellState cell
      gen = cellRand cell
      prob = probability conf
      illDays = illnessDuration conf
      incDays = incubationPeriod conf
      immDays = immunityDuration conf
  in case state of
       Infected num -> handleState (Infected num) incDays gen
       Ill num      -> handleState (Ill num) illDays gen
       Immune num   -> handleState (Immune num) immDays gen
       Healthy      -> let count = infectedNeighbours g
                           p2beat = (1 - prob)^count
                           (randNum, newgen) = randomR (0, 1) gen 
                           cstate = if randNum < p2beat
                                    then Healthy
                                    else Infected 0
                        in Cell cstate newgen
  where
    handleState cstate per gen 
      = let ctx = if getPeriod cstate + 1 >= per 
                  then nextState cstate else addDay cstate
        in Cell ctx gen
    nextState ctx = case ctx of
      Healthy      -> Infected 0
      (Infected _) -> Ill 0
      (Ill _)      -> Immune 0
      (Immune _)   -> Healthy
    getPeriod ctx = case ctx of
      Healthy -> 0
      (Infected n) -> n
      (Ill n)      -> n
      (Immune n)   -> n
    addDay ctx = case ctx of
      Healthy      -> Healthy
      (Infected n) -> Infected $ n + 1
      (Ill n)      -> Ill $ n + 1
      (Immune n)   -> Immune $ n + 1

initGrid :: Int -> Comonad19Grid
initGrid seed 
  = let initGen = mkStdGen seed
        rndNums = randoms initGen :: [Int]
        ls = initCellsList $ filter odd rndNums
        rs = initCellsList $ filter even rndNums
        lz = LZ ls (Cell Healthy (mkStdGen seed)) rs
        lzlz = Grid $ duplicate lz
    in gridWrite (Cell (Infected 0) (mkStdGen seed)) lzlz
    where
      initCellsList = fmap $ \x -> Cell Healthy (mkStdGen x)

lz2ListByRange :: (Int, Int) -> ListZipper a -> [a]
lz2ListByRange (a, b) g 
  = let lz = if a <= 0 
           then iterate listLeft g !! (1 - a) 
           else iterate listRight g !! (a - 1)
    in take (b - a + 1) (tr lz)
  where
    tr (LZ _ _ rs) = rs

grid2List :: Int -> Grid a -> [[a]]
grid2List x = let x' = x `div` 2 
                  range = if even x then (-x', x' - 1) else (-x', x')
              in lz2ListByRange range . (fmap . lz2ListByRange $ range) . unGrid

renderComonad :: Int -> Comonad19Grid -> String
renderComonad gridSize = unlines . map (concatMap show) . grid2List gridSize

-- | Takes in the size (wanted view) of the grid and prints it
printGrid :: Int -> Comonad19Grid -> IO ()
printGrid sz g = putStrLn $ renderComonad sz g

-- | Creates an infinite list of grids using the given configuration.
-- Each element of this list represents one infection simulation step.
-- Takes in a seed and a config
simulate :: Int -> Config -> [Comonad19Grid]
simulate seed conf = iterate (extend $ rule conf) (initGrid seed)
 
