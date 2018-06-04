module Repl (runRepl) where

import Eval

import System.IO (stdout, hFlush)

runRepl :: IO ()
runRepl = do
  putStr "λ> "
  hFlush stdout
  line <- getLine
  case run line of
    Right result -> print result
    Left  err    -> putStrLn $ "Error: " ++ err
  runRepl
