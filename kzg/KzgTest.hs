{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll
module KZGTest where

import Curve (g1, g2, pairing, curveOrder)
import KZG
import Polynomial
import EllipticCurves
import Data.List
import Test.QuickCheck

-- checks [p(s)] + [q(s)] = [(p + q)(s)] in both groups G1 and G2
prop_combinedComm secret p1 p2 =
        comm1G1 `add` comm2G1 == comm3G1 && comm1G2 `add` comm2G2 == comm3G2
    where
    secret'        = abs secret
    (ptsG1, ptsG2) = trustedSetup secret' g1 g2
    poly1          = dropWhile (== 0) p1
    poly2          = dropWhile (== 0) p2
    poly3          = poly1 `polyadd` poly2
    comm1G1        = polycommit curveOrder ptsG1 poly1
    comm2G1        = polycommit curveOrder ptsG1 poly2
    comm3G1        = polycommit curveOrder ptsG1 poly3
    comm1G2        = polycommit curveOrder ptsG2 poly1
    comm2G2        = polycommit curveOrder ptsG2 poly2
    comm3G2        = polycommit curveOrder ptsG2 poly3

-- checks [p(s)] * [q(s)] = [(p * q)(s)] * g2
prop_mulComm secret p1 p2 =
        pairing comm2G2 comm1G1 == pairing g2 comm3G1 &&
        pairing comm1G2 comm2G1 == pairing comm3G2 g1
    where
    secret'        = abs secret
    (ptsG1, ptsG2) = trustedSetup secret' g1 g2
    poly1          = dropWhile (== 0) p1
    poly2          = dropWhile (== 0) p2
    poly3          = poly1 `polymult` poly2
    comm1G1        = polycommit curveOrder ptsG1 poly1
    comm2G2        = polycommit curveOrder ptsG2 poly2
    comm3G1        = polycommit curveOrder ptsG1 poly3
    comm1G2        = polycommit curveOrder ptsG2 poly1
    comm2G1        = polycommit curveOrder ptsG1 poly2
    comm3G2        = polycommit curveOrder ptsG2 poly3

-- NOTE: test are very slow due to pairings. May take about 30 minutes
prop_proveVerify secret poly z = verify (curveOrder, g1, g2, sG2, pairing) comm proof z y
    where
    -- ensures the inputs are good for randomized tests
    secret'        = 2 + abs secret
    poly'          = dropWhile (== 0) (poly ++ [1, 1])
    (ptsG1, ptsG2) = trustedSetup secret' g1 g2
    sG2            = ptsG2!!1
    comm           = polycommit curveOrder ptsG1 poly'
    proof          = prove (curveOrder, ptsG1) poly' z
    y              = polyevalGf curveOrder poly' z

prop_multiProveVerifyExample = multiVerify (curveOrder, g2, ptsG1, ptsG2, pairing) comm proof zs ys
    where
    secret         = 34
    poly           = [1, 1, 1, 2]
    (ptsG1, ptsG2) = trustedSetup secret g1 g2
    comm           = polycommit curveOrder ptsG1 poly
    zs             = [5, 20, 89]
    proof          = multiProve (curveOrder, ptsG1) poly zs
    ys             = map (polyevalGf curveOrder poly) zs

-- NOTE: test are very slow due to pairings. May take about 30 minutes
prop_multiProveVerify secret poly zs = length poly' > length uniqueZs && not (null uniqueZs) ==>
                                       multiVerify (curveOrder, g2, ptsG1, ptsG2, pairing) comm proof uniqueZs ys
    where
    secret'        = 2 + abs secret
    poly'          = dropWhile (== 0) (poly ++ [1, 1])
    (ptsG1, ptsG2) = trustedSetup secret' g1 g2
    comm           = polycommit curveOrder ptsG1 poly'
    uniqueZs       = nub zs
    proof          = multiProve (curveOrder, ptsG1) poly' uniqueZs
    ys             = map (polyevalGf curveOrder poly') uniqueZs

return []
runTests = $quickCheckAll
