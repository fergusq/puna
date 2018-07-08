module Main where

import System.Environment
import Puna

main :: IO ()
main = do [code] <- getArgs
          runProgram code
