-- based on https://dankradfeist.de/ethereum/2020/06/16/kate-polynomial-commitments.html
-- IMPORTANT: polynomial coefficients and argument x must belong to group Z_p, where
-- p is the curve order
module KZG where

import EllipticCurves
import Polynomial
import Field
import Gf

-- TODO: if possible, make polynomials / elliptic points monoids / semigroups in Haskell
-- so that they can work with generic functions like `sum`
trustedSetup secret g1 g2 = (getPowerCycle g1 secret, getPowerCycle g2 secret)

-- converts a list of integrals to Fp where p = curver order
toGf curveOrder = map ((`intGf` curveOrder).fromIntegral)

-- computes Kate commitment (it's like a "hash" of a polynomial): C = [p(s)]1
-- Kate commitments work similarly to polynomial evaluation, but now
-- x^i is g * secret^i. However, since the points `ptsEc` are public, nobody
-- needs to know the actual `secret` to evaluate the polynomial `poly`.
-- There may be many polynomials evaluating to the same point, but, as with
-- hashes, it's hard to find collisions
polycommit curveOrder ptsEc poly = foldl add O $ zipWith mul ptsEc polyGf
    where
    polyGf = toGf curveOrder poly

polyevalGf curveOrder poly x = polyeval polyGf xGf
    where
    polyGf = toGf curveOrder poly
    xGf    = intGf (fromIntegral x) curveOrder

-- q(x) = p(x) - y / (x - z), i.e., division has no remainder
commitQ curveOrder ptsEc poly ip zp
    | not $ null r' = error "division problem"
    | otherwise     = polycommit curveOrder ptsEc q
    where
    (q, r) = (poly `polysub` ip) `polydiv` zp
    r'     = dropWhile (== 0) r

-- Kate proof for the evaluation p(z) = y is defined as [q(s)]1
prove (curveOrder, ptsEc) poly z = commitQ curveOrder ptsEc polyGf [y] [-zGf, toOne zGf]
    where
    polyGf = toGf curveOrder poly
    zGf    = intGf (fromIntegral z) curveOrder
    y      = polyGf `polyeval` zGf

-- verifies a Kate proof where commitment = [p(s)]1, s = secret * G2,
-- and p(z) = y. Checks: e([s-z]2, proof) == e(g2, commitment - [y]1)
verify (curveOrder, g1, g2, sG2, pairing) commitment proof z y = e1 == e2
    where
    zGf = intGf (fromIntegral z) curveOrder
    yG1 = g1 `mul` y
    zG2 = g2 `mul` zGf
    e1  = pairing (sG2 `sub` zG2) proof
    e2  = pairing g2 (commitment `sub` yG1)

-- Kate proof for multiple points `zs`. The degree of `poly` must be
-- higher than the number of points
multiProve (curveOrder, ptsEc) poly zs
    | length poly > length zs = commitQ curveOrder ptsEc polyGf ip zp
    | otherwise               = error "too many points"
    where
    polyGf = toGf curveOrder poly
    one    = toOne (head polyGf)
    zsGf   = toGf curveOrder zs
    ys     = map (polyeval polyGf) zsGf
    ip     = lagrangeGeneric one zsGf ys
    zp     = zPolyGeneric one zsGf

multiVerify (curveOrder, g2, ptsG1, ptsG2, pairing) commitment proof zs ys = e1 == e2
    where
    zsGf = toGf curveOrder zs
    one  = toOne (head zsGf)
    zp   = zPolyGeneric one zsGf
    zsG2 = polycommit curveOrder ptsG2 zp
    ip   = lagrangeGeneric one zsGf ys
    isG1 = polycommit curveOrder ptsG1 ip
    e1   = pairing zsG2 proof
    e2   = pairing g2 (commitment `sub` isG1)