module Main where

import Py2Hs
import Language.Python.Version3.Parser

main :: IO ()
main = do
  exampleFile <- readFile "examples/example.py"
  case parseModule exampleFile "example" of
    Left err -> print err
    Right x -> do
      let transpiled = transpileParsed x
      putStrLn transpiled
      writeFile "examples/Example.hs" transpiled
