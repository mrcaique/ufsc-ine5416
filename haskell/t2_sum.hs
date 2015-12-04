-- Module Sum
-- This module calculates the sum from some series,
-- for example, odd numbers, even numbers, squared
-- numbers, etc..
--
-- :author: Caique Marques
module Sum  (
                oddIntegerSeries, oddIntegerSum,
                evenIntegerSeries, evenIntegerSum,
                squareSeries, squareSum,
                almostTwo, almostEuler
            ) where

-- Infinite series and sum of odd numbers.
oddIntegerSeries n = map(\x -> x*((2*x-1) + 1)/2) [1..n]
oddIntegerSum n = last(oddIntegerSeries n)

-- Infinite series and sum of even numbers.
evenIntegerSeries n = map(\x -> x*(2*x + 2)/2) [1..n]
evenIntegerSum n = last(evenIntegerSeries n)

-- Infinite series and sum of squared numbers.
squareSeries n = map(\x -> x*(x+1)*(2*x+1)/6) [1..n]
squareSum n = last(squareSeries n)

-- Infinite series and sum of even squared numbers.
evenSquareSeries n = map(\x -> x*(4 * x^2 - 1)/3) [1..n]
evenSquareSum n = last(evenSquareSeries n)

-- Sum for approximation of the number two
almostTwo n = sum(map(\x -> 2/(x+x^2)) (take n [1..]))

-- Sum for approximation of Euler's number (e).
almostEuler n = sum(map(\x -> 1/product[1..x]) (take n [1..]))
