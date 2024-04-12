module FFT where

-- implementation of FFT following: https://vitalik.ca/general/2019/05/12/fft.html

import Data.Sequence(unfoldr, replicate, (><))
import Data.Foldable
import Basic
import ModularArithmetic

-- splits a list into two sublists: one with even-index and one with odd-index elements
split [] = ([], [])
split [p1] = ([p1], [])
split (p1:p2:ps) = (p1:evens, p2:odds)
    where
    (evens, odds) = split ps

-- dot product of a and b modulo m
dotProductMod m a b = dotProduct a b `mod` m

-- FT optimized for up to 4 elements
simpleFt [] _ _ = []
simpleFt vals m [r] = [dotProductMod m vals [r]]
simpleFt vals m [r0, r1] = map (dotProductMod m vals) [[r0, r0], [r0, r1]]
simpleFt vals m [r0, r1, r2] = map (dotProductMod m vals) [[r0, r0, r0], [r0, r1, r2], [r0, r2, r1]]
simpleFt vals m [r0, r1, r2, r3] = map (dotProductMod m vals) [[r0, r0, r0, r0], [r0, r1, r2, r3], [r0, r2, r0, r2], [r0, r3, r2, r1]]

-- evaluates a polynomial on all elements of the roots of unity at once
fft [v] _ _ = [v]
fft polynomial m rootsOfUnity
    | length polynomial <= 4 = simpleFt polynomial m rootsOfUnity
    | otherwise              = ls ++ rs
    where
    (evens, odds) = split polynomial
    (d, _) = split rootsOfUnity
    l = fft evens m d
    r = fft odds m d
    lrd = zip3 l r rootsOfUnity
    (ls, rs) = unzip [((x + yr) `mod` m, (x - yr) `mod` m) | (x, y, root) <- lrd, let yr = y * root]

-- run the FFT again, but reverse the result (except the first item stays in place) and
-- divide every value by the number of polynomial coefficients
inverseFft polynomial m rootsOfUnity = [(x * inv (fromIntegral $ length polynomial) m) `mod` m | x <- p : reverse ps]
    where
    (p:ps) = fft polynomial m rootsOfUnity

-- main convenience FFT function that can do a regular FFT or an inverse
fftMain vals m rootOfUnity isInv
    | isInv     = inverseFft vals' m roots
    | otherwise = fft vals' m roots
    where
    roots = getPowerCycle rootOfUnity m
    vals' = vals ++ Prelude.replicate (length roots - length vals) 0

-- evaluates f(x) for f in evaluation form
inverseFftAtPoint [v] _ _ _ = v
inverseFftAtPoint vals m rootOfUnity x = inverseFftAtPoint halfComb m (rootOfUnity^2 `mod` m) (x^2 `mod` m)
    where
    -- 1/2 in the field
    half = (m + 1) `div` 2
    len  = length vals
    -- 1/w
    invRoot = pow rootOfUnity (len - 1) m
    -- f(-x) in evaluation form
    fOfMinusXVals = uncurry (++) $ swap $ splitAt (len `div` 2) vals
    -- e(x) = (f(x) + f(-x)) / 2 in evaluation form
    evens = [(f + g) * half `mod` m | (f, g) <- zip vals fOfMinusXVals]
    -- o(x) = (f(x) - f(-x)) / 2 in evaluation form
    odds  = [(f - g) * half `mod` m | (f, g) <- zip vals fOfMinusXVals]
    -- e(x^2) + coordinate * x * o(x^2) in evaluation form
    comb = [(o * x * invRoot^i + e) `mod` m | (i, o, e) <- zip3 [0..] odds evens]
    halfComb = take (length comb `div` 2) comb

shiftDomain [v] _ _ _ = [v]
shiftDomain vals m rootOfUnity factor = uncurry (++) $ unzip
    [((e + invFactor * o) `mod` m, (e - invFactor * o) `mod` m ) | (e, o) <- zip shiftedEvens shiftedOdds]
    where
    -- 1/2 in the field
    half = (m + 1) `div` 2
    -- 1/w
    invFactor = pow factor (m - 2) m
    halfLen   = length vals `div` 2
    -- f(-x) in evaluation form
    fOfMinusXVals = uncurry (++) $ swap $ splitAt halfLen vals
    -- e(x) = (f(x) + f(-x)) / 2 in evaluation form
    evens = [(f + g) * half `mod` m | (f, g) <- zip vals fOfMinusXVals]
    -- o(x) = (f(x) - f(-x)) / 2 in evaluation form
    odds  = [(f - g) * half `mod` m | (f, g) <- zip vals fOfMinusXVals]
    shiftedEvens = shiftDomain (take halfLen evens) m (rootOfUnity^2 `mod` m) (factor^2 `mod` m)
    shiftedOdds  = shiftDomain (take halfLen odds)  m (rootOfUnity^2 `mod` m) (factor^2 `mod` m)

shiftPoly poly m factor = tail $ map snd (scanl
    (\(factorPower, os) p -> (factorPower * invFactor `mod` m, p * factorPower `mod` m)) (1, 0) poly)
    where
    invFactor = pow factor (m - 2) m

polymultFft a b m rootOfUnity = fft [(v1 * v2) `mod` m | (v1, v2) <- zip x1 x2] m rootz
    where
    roots = getPowerCycle rootOfUnity m
    a' = a ++ Prelude.replicate (length roots - length a) 0
    b' = b ++ Prelude.replicate (length roots - length b) 0
    x1 = fft a' m roots
    x2 = fft b' m roots
    rootz = head roots : reverse (tail roots)

-- convert number (with base b) to a sequence
decToSeq b = unfoldr (\n -> if n == 0 then Nothing else Just $ swap $ n `divMod` b)

-- appends the element e to extend the list to a given length
extend xs len e = toList (xs >< Data.Sequence.replicate (len - length xs) e)

-- converts a number to an extended list (required for FFT)
numToExList base n d = extend (decToSeq base n) (length d) 0

-- carries tens digits over to normalize the number
carry m = carry' m 0

carry' m 0 [] = []
carry' m c [] = [c]
carry' m c (n:ns) = r : carry' m q ns
    where
    (q, r) = (n + c) `divMod` m

-- convers a list of digits to a number in a given base
listToNumber b ns = foldl (\acc x -> acc * b + x) 0 $ reverse ns

-- quick number multiplication:
-- multiply numbers a and b (with digits in a given base)
-- so that g is a generator for the prime field with modulus m
-- Step 1: Take the polynomials representing our two numbers in the coefficient form,
-- Step 2: Use FFTs to convert them to evaluation form
-- Step 3: Multiply them pointwise
-- Step 4: Convert back to the polynomial form
-- Step 5: Carry the tens digits over
-- Step 6: Convert the reversed list into a number
quickMultiply base a b g m = listToNumber base $ carry base $ inverseFft p3 m d
    where
    d = getPowerCycle g m
    p1 = fft (numToExList base a d) m d
    p2 = fft (numToExList base b d) m d
    p3 = [(v1 * v2) `mod` m | (v1, v2) <- zip p1 p2]
