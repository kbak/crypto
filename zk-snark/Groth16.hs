module Groth16 where
-- bases on https://www.rareskills.io/post/groth16
import KZG
import EllipticCurves
import Gf
import Field
import ZkSnark
import Polynomial
import Data.List

-- trusted setup creates a short summary of the circuit given as (aQap, bQap, cQap, z)
-- the verifier cannot simply read the circuit on its own, since we want the verification
-- to be succinct
trustedSetup curveOrder secret g1 g2 alpha beta gamma delta pubLen (aQap, bQap, cQap, z) =
        (ptsG1, ptsG2, alphaG1, betaG1, betaG2, gammaG2, deltaG1, deltaG2, cPts', tPts)
    where
    (ptsG1, ptsG2) = KZG.trustedSetup secret g1 g2
    alphaG1        = g1 `mul` alpha
    betaG1         = g1 `mul` beta
    betaG2         = g2 `mul` beta
    gammaG2        = g2 `mul` gamma
    deltaG1        = g1 `mul` delta
    deltaG2        = g2 `mul` delta
    aBeta          = matrixscale (intGf beta  curveOrder) aQap
    bAlpha         = matrixscale (intGf alpha curveOrder) bQap
    cQap'          = aBeta `matrixadd` bAlpha `matrixadd` cQap
    cPts           = map (polycommit curveOrder ptsG1) $ transpose cQap'
    gammaInv       = inv (intGf gamma curveOrder)
    deltaInv       = inv (intGf delta curveOrder)
    (cPub, cPriv)  = splitAt pubLen cPts
    pubPts         = map (`mul` gammaInv) cPub
    privPts        = map (`mul` deltaInv) cPriv
    cPts'          = pubPts ++ privPts
    zSecretEval    = polyevalGf curveOrder z secret
    tPts           = map (`mul` (zSecretEval * deltaInv)) ptsG1

prove (curveOrder, ptsG1, ptsG2, alphaG1, betaG1, betaG2, deltaG1, deltaG2, cPts, tPts) (aQap, bQap, cQap, z) (witness, pubLen) r s
    | isZero h  = (aG1, bG2, cG1)
    | otherwise = error "invalid QAP"
    where
    witnessGf    = toGf curveOrder witness
    (a, b, _, t) = computePolysGf curveOrder (aQap, bQap, cQap) witnessGf
    -- evaluates each polynomial over the set of points from the trusted setup
    aPt    = polyevalEnc curveOrder a ptsG1
    bPt1   = polyevalEnc curveOrder b ptsG1
    bPt2   = polyevalEnc curveOrder b ptsG2
    cPt    = polyevalEnc curveOrder (drop pubLen witnessGf) (drop pubLen cPts)
    (q, h) = t `polydiv` z
    tPt    = polyevalEnc curveOrder q tPts
    rG1    = deltaG1 `mul` r
    sG2    = deltaG2 `mul` s
    sG1    = deltaG1 `mul` s
    aG1    = aPt `add` alphaG1 `add` rG1
    bG1    = bPt1 `add` betaG1 `add` sG1
    bG2    = bPt2 `add` betaG2 `add` sG2
    rsG1   = deltaG1 `mul` (r * s)
    cG1    = cPt `add` tPt `add` (aG1 `mul` s) `add` (bG1 `mul` r) `add` neg rsG1

-- IMPORTANT: pairings need to be multiplied, not added
-- it takes only the public part of the witness
verify (curveOrder, alphaG1, betaPt, gammaG2, deltaG2, cPts, pairing) proof witness = e1 == e2 * e3 * e4
    where
    (e1, e2)  = pair (deltaG2, pairing) proof
    e3        = pairing betaPt alphaG1
    witnessGf = toGf curveOrder witness
    cPt       = polyevalEnc curveOrder witnessGf cPts
    e4        = pairing gammaG2 cPt
