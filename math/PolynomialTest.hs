{-# LANGUAGE TemplateHaskell #-} 
-- ^^^ for QuickCheck.quickCheckAll
module PolynomialTest where

import Basic
import Polynomial
import Data.List
import Test.QuickCheck

-- some vector
vStr = "ZKSUMMIT"

vNumbers = map fromEnum vStr

-- polynomial where p(i+1) = vStr[i] (after rounding)
poly = lagrange (map fromIntegral [1..]) (map fromIntegral vNumbers)

prop_lagrange = head vStr == (toEnum (round (polyeval poly 1.0) :: Int) :: Char)

prop_poly4 ps x = length ps >= 4 ==> polyeval4 ps' x == polyeval ps' x
    where
    ps' = take 4 ps

prop_lagrange2 xs ys = length nXs >= 2 && length ys >= 2 ==>
    map rf res1 == map rf res2
    where
    nXs = nub xs
    xs' = map fromIntegral $ take 2 nXs
    ys' = map fromIntegral $ take 2 ys
    res1 = lagrange2 xs' ys'
    res2 = lagrange  xs' ys'
    -- we need to round the floating point results
    rf = roundFrac 5

prop_lagrange4 xs ys = length nXs >= 4 && length ys >= 4 ==>
    map rf res1 == map rf res2
    where
    nXs = nub xs
    xs' = map fromIntegral $ take 4 nXs
    ys' = map fromIntegral $ take 4 ys
    res1 = lagrange4 xs' ys'
    res2 = lagrange  xs' ys'
    -- we need to round the floating point results
    rf = roundFrac 1

return []
runTests = $quickCheckAll