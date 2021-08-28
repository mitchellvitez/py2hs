module BinarySearch where

import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

range :: Enum a => a -> a -> [a]
range m n = [m..n]

-- https://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python
takeStep :: Int -> [a] -> [a]
takeStep _ [] = []
takeStep n (x:xs)
  | n >= 0 = x : takeStep n (drop (n-1) xs)
  | otherwise = takeStep (-n) (reverse xs)

slice :: Int -> Int -> Int -> [a] -> [a]
slice a e d xs = z . y . x $ xs -- a:start, e:stop, d:step
  where a' = if a >= 0 then a else (length xs + a)
        e' = if e >= 0 then e else (length xs + e)
        x = if d >= 0 then drop a' else drop e'
        y = if d >= 0 then take (e'-a') else take (a'-e'+1)
        z = takeStep d

lessThanOne x =
  if x < 1
  then True
  else False

fibonacci n =
  if n <= 1
  then n
  else fibonacci (n - 1) + fibonacci (n - 2)

fibonacci2 n =
  if n <= 1
  then n
  else fibonacci2 (n - 1) + fibonacci2 (n - 2)

allPairs arrA arrB =
  [ (a, b) | a <- arrA, b <- arrB ]

plusOne x =
  x + 1

plusOneLambda  =
  \x -> x + 1

factorial n =
  if n == 0
  then 1
  else n * factorial (n - 1)

squares n =
  [ x ^ 2 | x <- range 1 n ]

pythagoreanTriples n =
  [ (a, b, c) | a <- range 1 n, b <- range 1 n, c <- range 1 n ]

quadraticFormula a b c =
  let d = b ^ 2 - 4 * a * c in
  let ans1 = (-b - sqrt d) / (2 * a) in
  let ans2 = (-b + sqrt d) / (2 * a) in
  (ans1, ans2)