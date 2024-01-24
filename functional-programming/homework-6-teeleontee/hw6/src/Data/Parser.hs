module Data.Parser 
( Opts (..)
, getOptionsAndConfig
) where

import HW6.T3

import Control.Exception (Exception, throw)
import Control.Monad
import Options.Applicative as O

data ParseException 
  = ProbabilityException Comomonad19OptsEnum 
  | NegativePeriodException Comomonad19OptsEnum
  | InvalidGridException Comomonad19OptsEnum
  | InvalidIterationException Comomonad19OptsEnum

instance Show ParseException where
  show (ProbabilityException opt)       = "Probability should be between 0 and 1 => " ++ show opt
  show (NegativePeriodException opt)    = "The Period should not be negative => " ++ show opt
  show (InvalidGridException opt)       = "Cannot create a grid with => " ++ show opt
  show (InvalidIterationException opt)  = "Cannot have given Iteration => " ++ show opt

instance Exception ParseException

data Comomonad19OptsEnum 
 = Probability Double 
 | IncubationPeriod Int 
 | IllnessPeriod Int 
 | ImmunityPeriod Int
 | GridSize Int 
 | Iterations Int

instance Show Comomonad19OptsEnum where
  show (Probability num) = "--prob " ++ show num
  show (IncubationPeriod num) = "--inc " ++ show num
  show (ImmunityPeriod num) = "--imm " ++ show num
  show (IllnessPeriod num) = "--ill " ++ show num
  show (GridSize num) = "--grid-size " ++ show num
  show (Iterations num) = "--iterations " ++ show num

data Opts = Opts 
  { optProbability :: !Double
  , optIncPeriod :: !Int
  , optIllPeriod :: !Int
  , optImmPeriod :: !Int
  , optGridSize :: !Int
  , optIterations :: !Int
  }

optsParser :: ParserInfo Opts
optsParser = info 
  (helper <*> programOptions)
  (fullDesc <> progDesc "Comonad 19 Simulation" <>
   header
     "Comonad19 - a small program that simulates the propogation of a certian virus...")

programOptions :: Parser Opts
programOptions = 
  Opts <$> O.option auto (long "prob" <> metavar "Double" <> value 0.30 <> help "Probabilty of infection")
       <*> O.option auto (long "incub" <> metavar "Int" <> value 3 <> help "Incubation period")
       <*> O.option auto (long "ill" <> metavar "Int" <> value 3 <> help "Illness period")
       <*> O.option auto (long "immun" <> metavar "Int" <> value 3 <> help "Immunity period")
       <*> O.option auto (long "grid-size" <> metavar "Int" <> value 21 <> help "Size of the grid")
       <*> O.option auto (long "iterations" <> metavar "Int" <> value 15 <> help "Amount of days simulated")

option2Config :: Monad m => Opts -> m Config
option2Config ops = 
  let probs  = optProbability ops
      incPer = optIncPeriod ops
      illPer = optIllPeriod ops
      immPer = optImmPeriod ops
      gridSz = optGridSize ops
      iter   = optIterations ops
  in do
    unless (probs <= 1 && probs > 0) (throw $ ProbabilityException $ Probability probs)
    unless (incPer >= 0) (throw $ NegativePeriodException $ IncubationPeriod incPer)
    unless (illPer >= 0) (throw $ NegativePeriodException $ IllnessPeriod illPer)
    unless (immPer >= 0) (throw $ NegativePeriodException $ ImmunityPeriod immPer)
    unless (gridSz >= 1) (throw $ InvalidGridException $ GridSize gridSz)
    unless (iter >= 0)   (throw $ InvalidIterationException $ Iterations iter)
    return $ Config probs incPer illPer immPer 

getOptionsAndConfig :: IO (Opts, Config)
getOptionsAndConfig = do
  opts <- execParser optsParser
  conf <- option2Config opts
  return (opts, conf)
