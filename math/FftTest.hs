{-# LANGUAGE TemplateHaskell #-} 
-- ^^^ for QuickCheck.quickCheckAll

module FFTTest where

import ModularArithmetic
import Polynomial
import FFT -- if ghci fails, run `:set -ifft`
import Test.QuickCheck

dom = getPowerCycle generator modulus

-- polynomial
p = [3, 1, 4, 1, 5, 9, 2, 6]

modulus = 337

generator = 85

prop_fft = [31, 70, 109, 74, 334, 181, 232, 4] == fft p modulus dom

prop_inverseFft = p == inverseFft (fft p modulus dom) modulus dom

prop_fftMain = fftMain p modulus generator False == fft p modulus dom

prop_fftMainInv = [46, 169, 29, 149, 126, 262, 140, 93] == fftMain p modulus generator True

prop_fftEval = p == map (\v -> v `mod` modulus) values
    where
    values = map (polyeval $ fftMain p modulus generator True) [pow generator (i :: Integer) modulus | i <- [0 .. 7]]

prop_inverseAtPoint = 267 == inverseFftAtPoint p modulus generator 2

prop_shiftDomain = [195, 243, 214, 287, 125, 85, 187, 43] == shiftDomain p modulus generator 2

prop_shiftPoly = [3, 169, 1, 295, 232, 74, 158, 237] == shiftPoly p modulus 2

prop_polymultFft = [256, 280, 184, 144, 232, 296, 71, 95] == polymultFft p [2, 3, 4] modulus generator

n1 = 1253
n2 = 1895

numBase = 10 :: Integer

-- Step 1
p1 = numToExList numBase n1 dom
p2 = numToExList numBase n2 dom

-- Step 2
x1 = fft p1 modulus dom
x2 = fft p2 modulus dom

prop_x1 = [11, 161, 256, 10, 336, 100, 83, 78] == x1

prop_x2 = [23, 43, 170, 242, 3, 313, 161, 96] == x2

-- Step 3
x3 = [(v1 * v2) `mod` modulus | (v1, v2) <- zip x1 x2]

prop_x3 = [253, 183, 47, 61, 334, 296, 220, 74] == x3

-- Step 4
x4 = inverseFft x3 modulus dom

prop_x4 = [15, 52, 79, 66, 30, 10, 1, 0] == x4

-- Step 5
x5 = carry numBase x4

prop_x5 = x5 == [ 5,  3,  4,  4,  7,  3, 2, 0]

-- Step 6
x6 = listToNumber numBase x5

prop_x6 = x6 == n1 * n2

prop_quickMultiply = n1 * n2 == quickMultiply numBase n1 n2 generator modulus

return []
runTests = $quickCheckAll