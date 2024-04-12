module Polynomial where

-- from https://hackage.haskell.org/package/dsp-0.2.4/docs/src/Polynomial-Basic.html

-- | Polynomials are lists of numbers:
-- [ a0, a1, ... , an ] == an*x^n + ... + a1*x + a0

-- polynomial evaluation
polyeval []     _ = 0
polyeval (p:ps) x = p + x * polyeval ps x

-- checks if the coefficients are all zero
isZero cs = null $ dropWhile (== 0) cs

-- cleans up leading zero's from the polynomial's coefficients
coeffclean p = reverse $ dropWhile (== 0) $ reverse p

-- polynomial evaluation optimized for deg-4 polynomials
polyeval4 [p0, p1, p2, p3] x = p0 + p1 * x + p2 * xsq + p3 * xcb
    where
    xsq = x * x
    xcb = xsq * x

-- | Add two polynomials
polyadd [] ys          = ys
polyadd xs []          = xs
polyadd (x:xs) (y:ys)  = (x + y) : polyadd xs ys

-- | Subtract two polynomials

polysub [] ys          = Prelude.map negate ys
polysub xs []          = xs
polysub (x:xs) (y:ys)  = (x - y) : polysub xs ys

-- | Scale a polynomial

polyscale a = Prelude.map (a*)

matrixadd :: Num a => [[a]] -> [[a]] -> [[a]]
matrixadd = zipWith polyadd

matrixscale a = map (polyscale a)

-- | Multiply two polynomials

polymult ys = Prelude.foldr (\x acc -> polyadd (polyscale x ys) (0 : acc)) []

-- | Divide two polynomials

polydiv x0 y0 = (reverse quotient, reverse remainder)
    where
    (quotient, remainder) = polydiv' (reverse x0) (reverse y0)
    polydiv' (x:xs) y
        | length (x:xs) < length y = ([], x:xs)
        | otherwise = (z : qs, r)
        where
        z = x / head y
        m = polymult [z] y
        (qs, r) = polydiv' (tail (polysub (x:xs) m)) y
    polydiv' [] _ = ([], [])

-- creates a the polynomial Z = (x-z1) * (x-z2) * ... * (x-zn)
zPoly :: Num a => [a] -> [a]
zPoly = zPolyGeneric 1

zPolyGeneric one zs = Prelude.foldl polymult [one] [[-z, one] | z <- zs]

-- for a set points, finds a Lagrange interpolation polynomial
-- i.e., a polynomial that that includes all these points
-- such that p(x1) = y[0], p(x2) = y[1], ... p(xn) = y[n-1]
lagrangeGeneric one xs ys = Prelude.foldl polyadd [] singletons
    where
    points = zip xs ys
    singletons = Prelude.map mkSingleton points
    -- creates a singleton polynomial for Lagrange interpolation
    -- which is zero at {x1, x2 ... xn}, except for `xi` where the value is `yi`,
    -- i.e., p(x1) = 0, p(x2) = 0, ... p(xi) = yi, ... p(xn) = 0
    mkSingleton p@(x, y) = polymult [y / factor] (zPolyGeneric one pointsWithout)
        where
        pointsWithout = Prelude.map fst $ filter (/= p) points
        -- we need to divide the coefficients because otherwise y
        -- would be multiplied by all other factors
        factor = foldl (*) one $ Prelude.map (x -) pointsWithout

lagrange :: (Fractional a, Eq a) => [a] -> [a] -> [a]
lagrange = lagrangeGeneric 1

-- optimized version of the above restricted to deg-2 polynomials
lagrange2 [x0, x1] [y0, y1] = [-x1 * invY0 - x0 * invY1, invY0 + invY1]
    where
    eq0 = [-x1, 1]
    eq1 = [-x0, 1]
    e0  = polyeval eq0 x0
    e1  = polyeval eq1 x1
    invall = 1 / (e0 * e1)
    invY0  = y0 * invall * e1
    invY1  = y1 * invall * e0

-- optimized version of the above restricted to deg-4 polynomials
lagrange4 [x0, x1, x2, x3] [y0, y1, y2, y3] = [
    eq00 * invY0 + eq10 * invY1 + eq20 * invY2 + eq30 * invY3,
    eq01 * invY0 + eq11 * invY1 + eq21 * invY2 + eq31 * invY3,
    eq02 * invY0 + eq12 * invY1 + eq22 * invY2 + eq32 * invY3,
    eq03 * invY0 + eq13 * invY1 + eq23 * invY2 + eq33 * invY3 ]
    where
    x01 = x0 * x1
    x02 = x0 * x2
    x03 = x0 * x3
    x12 = x1 * x2
    x13 = x1 * x3
    x23 = x2 * x3
    eq0@[eq00, eq01, eq02, eq03] = [-x12 * x3, x12 + x13 + x23, -x1 - x2 - x3, 1]
    eq1@[eq10, eq11, eq12, eq13] = [-x02 * x3, x02 + x03 + x23, -x0 - x2 - x3, 1]
    eq2@[eq20, eq21, eq22, eq23] = [-x01 * x3, x01 + x03 + x13, -x0 - x1 - x3, 1]
    eq3@[eq30, eq31, eq32, eq33] = [-x01 * x2, x01 + x02 + x12, -x0 - x1 - x2, 1]
    e0 = polyeval4 eq0 x0
    e1 = polyeval4 eq1 x1
    e2 = polyeval4 eq2 x2
    e3 = polyeval4 eq3 x3
    e01 = e0 * e1
    e23 = e2 * e3
    invall = 1 / (e01 * e23)
    invY0 = y0 * invall *  e1 * e23
    invY1 = y1 * invall *  e0 * e23
    invY2 = y2 * invall * e01 * e3
    invY3 = y3 * invall * e01 * e2
