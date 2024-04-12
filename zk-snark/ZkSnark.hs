module ZkSnark where

import EllipticCurves
import KZG
import Polynomial
import Gf
import Data.List

-- encrypted polynomial evaluation (reuses polynomial commitment from KZG)
-- for a polynomial and a set of points from the trusted setup, evaluates
-- polynomial over (sG, i.e., generator muliplied by a secret `s`)
polyevalEnc curveOrder poly pts = polycommit curveOrder pts poly

-- some of the QAP-related functions had to be reimplemented to make them
-- compatible with Gf numbers, specifically the R1CS->QAP->Polynomials transformation
r1csToQapGf curveOrder (aR1cs, bR1cs, cR1cs) = (aQap, bQap, cQap, z)
    where
    oneGf          = intGf 1 curveOrder
    matrixToGf     = map (toGf curveOrder)
    matrixToQap m  = transpose $ map (lagrangeGeneric oneGf (toGf curveOrder [1..])) (transpose m)
    aQap           = matrixToQap $ matrixToGf aR1cs
    bQap           = matrixToQap $ matrixToGf bR1cs
    cQap           = matrixToQap $ matrixToGf cR1cs
    z              = zPolyGeneric oneGf $ toGf curveOrder [1 .. fromIntegral $ length aR1cs]

dotProductGf curveOrder a s = foldl (+) zero $ zipWith (*) a s
    where
    zero = intGf 0 curveOrder

dotProductQAP curveOrder m s = map (\v -> dotProductGf curveOrder v (map fromIntegral s)) m

computePolysGf curveOrder (aQap, bQap, cQap) s = (as, bs, cs, ts)
    where
    as  = dotProductQAP curveOrder aQap s
    bs  = dotProductQAP curveOrder bQap s
    cs  = dotProductQAP curveOrder cQap s
    ts  = as `polymult` bs `polysub` cs

-- creates a ZK-SNARK based for a program in the R1CS form based on:
-- 1) R1CS matrices (a, b, c),
-- 2) witness,
-- 3) points from the trusted setup.
-- Here we use the polynomial t instead of z since `computePolys` always produces a valid t,
-- i.e., a one that can be divided by z
-- IMPORTANT: we need to convert polynomials to GF so that they are evaluated
-- over the curve order modulus!
prove (curveOrder, ptsG1, ptsG2) (aR1cs, bR1cs, cR1cs) witness
    | isZero $ snd $ t `polydiv` z = (aPt, bPt, cPt `add` tPt)
    | otherwise                    = error "invalid QAP"
    where
    (aQap, bQap, cQap, z) = r1csToQapGf curveOrder (aR1cs, bR1cs, cR1cs)
    (a, b, c, t)          = computePolysGf curveOrder (aQap, bQap, cQap) (toGf curveOrder witness)
    -- evaluates each polynomial over the set of points from the trusted setup
    aPt = polyevalEnc curveOrder a ptsG1
    bPt = polyevalEnc curveOrder b ptsG2
    cPt = polyevalEnc curveOrder c ptsG1
    tPt = polyevalEnc curveOrder t ptsG1

-- NOTE: as is, the prover can still cheat by inventing the points.
-- They are not bound to QAP (yet)
verify publicSetup proof = e1 == e2
    where
    (e1, e2) = pair publicSetup proof

pair (g2, pairing) (aPt, bPt, qPt) = (pairing bPt aPt, pairing g2 qPt)