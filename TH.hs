{-# LANGUAGE QuasiQuotes, NoMonomorphismRestriction #-}

module Main where

import JTL.Value as V
import Text.JSON
import JTL.TH (expr)

filterBetween :: Int -> Int -> [Int] -> [Int]
filterBetween = [expr| @[@0 <= @ <= @1]::array |]

countBetween :: Int -> Int -> [Int] -> Int
countBetween = [expr| @[@0 <= @ <= @1]::count |]

getMax = [expr| *::max |]

getMin = [expr| *::min |]

main = putStrLn $ show $ countBetween 1 5 [0..8]
