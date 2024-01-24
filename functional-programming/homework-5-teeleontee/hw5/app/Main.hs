module Main (main) where

import HW5.Parser
import HW5.Evaluator (eval)
import HW5.Pretty (prettyValue)
import HW5.Action

import System.Console.Haskeline

import Data.Set
import Control.Monad.Cont
 
main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> case parse input of
                               Left e    -> outputStrLn $ show e
                               Right val -> do
                                              res <- liftIO $ runHIO (eval val) (fromList [AllowRead, AllowWrite, AllowTime])
                                              case res of
                                                Left e  -> outputStrLn $ show e
                                                Right r -> outputStrLn $ show $ prettyValue r
                                              loop
 
