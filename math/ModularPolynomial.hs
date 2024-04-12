module ModularPolynomial where

import ModularArithmetic
import Polynomial
import Data.List

-- optimized version polynomial interpolation restricted to deg-4 modulo polynomials but for multiple sets of points
multiLagrange4 xsets ysets m = zipWith (curry (interp4 m)) datasets invs
    where
    (datasets, invtargets) = unzip $ zipWith (curry (prepLagrange4data m)) xsets ysets
    invalls = multiInv (concat invtargets) m
    invs    = unfoldr (\l -> let (h, t) = splitAt 4 l in if null l then Nothing else Just (h, t)) invalls

    prepLagrange4data m series@([x0, x1, x2, x3], [y0, y1, y2, y3]) = ((snd series, eq0, eq1, eq2, eq3), [e0, e1, e2, e3])
        where
        x01 = x0 * x1
        x02 = x0 * x2
        x03 = x0 * x3
        x12 = x1 * x2
        x13 = x1 * x3
        x23 = x2 * x3
        eq0 = [-x12 * x3 `mod` m, x12 + x13 + x23, -x1 - x2 - x3, 1]
        eq1 = [-x02 * x3 `mod` m, x02 + x03 + x23, -x0 - x2 - x3, 1]
        eq2 = [-x01 * x3 `mod` m, x01 + x03 + x13, -x0 - x1 - x3, 1]
        eq3 = [-x01 * x2 `mod` m, x01 + x02 + x12, -x0 - x1 - x2, 1]
        e0 = polyeval4 eq0 x0
        e1 = polyeval4 eq1 x1
        e2 = polyeval4 eq2 x2
        e3 = polyeval4 eq3 x3

    interp4 m (([y0, y1, y2, y3], eq0, eq1, eq2, eq3), [e0, e1, e2, e3]) = [
        (eq00 * invY0 + eq10 * invY1 + eq20 * invY2 + eq30 * invY3) `mod` m,
        (eq01 * invY0 + eq11 * invY1 + eq21 * invY2 + eq31 * invY3) `mod` m,
        (eq02 * invY0 + eq12 * invY1 + eq22 * invY2 + eq32 * invY3) `mod` m,
        (eq03 * invY0 + eq13 * invY1 + eq23 * invY2 + eq33 * invY3) `mod` m ]
        where
        invY0 = y0 * e0 `mod` m
        invY1 = y1 * e1 `mod` m
        invY2 = y2 * e2 `mod` m
        invY3 = y3 * e3 `mod` m
        [eq00, eq01, eq02, eq03] = eq0
        [eq10, eq11, eq12, eq13] = eq1
        [eq20, eq21, eq22, eq23] = eq2
        [eq30, eq31, eq32, eq33] = eq3