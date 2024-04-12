{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll
module Groth16Test where
-- based on https://www.rareskills.io/post/groth16
import QAP
import Curve
import ZkSnark
import Groth16
import QAPTest
import ModularArithmetic
import EllipticCurves
import KZG
import Polynomial
import Data.List
import Test.QuickCheck

-- variables here have swapped positions of "x" and "~out" so that we can split
-- the vector into public and private inputs by indicating the length of the
-- public part (e.g., below it's going to be 2)
pubPrivVars = ["~one", "~out", "x", "sym_1", "y", "sym_2"]

qap@(aQap, bQap, cQap, gammaG2) = r1csToQapGf curveOrder $ codeToR1cs pubPrivVars flatCode
witness   = getWitness pubPrivVars assignment flatCode
witnessGf = toGf curveOrder Groth16Test.witness

-- checks that if for a QAP matrix we can either:
-- 1) compute a polynomial (through a dot product with witness) and then evaluate
--    it over encrypted points, or
-- 2) compute the encrypted polynomial (through a evaluation on encrypted points) and
--    then evaluate it over witness
-- to get the same result. This is useful since we can construct a trusted setup upfront
-- to evaluate a polynomial over the witness later.
prop_checkEvaluationOrder s = aPt == aPt'
    where
    secret       = abs s
    -- computing a polynomial first, and then evaluating over points
    (a, _, _, _) = computePolysGf curveOrder (aQap, bQap, cQap) witnessGf
    aPt          = polyevalEnc curveOrder a ptsG1
    -- computing points first and then evaluating over witness
    (ptsG1, _)   = KZG.trustedSetup secret g1 g2
    ptsMG1       = map (polycommit curveOrder ptsG1) $ transpose aQap
    aPt'         = polyevalEnc curveOrder witnessGf ptsMG1

-- checks addition of points
prop_checkPointOrder s = dPt == dPt' && dPt == dPt''
    where
    secret       = abs s
    (ptsG1, _)   = KZG.trustedSetup secret g1 g2
    -- encrypted point aPt + bPt + cPt
    (a, b, c, _) = computePolysGf curveOrder (aQap, bQap, cQap) witnessGf
    aPt          = polyevalEnc curveOrder a ptsG1
    bPt          = polyevalEnc curveOrder b ptsG1
    cPt          = polyevalEnc curveOrder c ptsG1
    dPt          = aPt `add` bPt `add` cPt
    -- encrypted point on polynomial (a + b + c)
    d            = a `polyadd` b `polyadd` c
    dPt'         = polyevalEnc curveOrder d ptsG1
    -- encrypted point on matrices (A + B + C)
    dQap         = aQap `matrixadd` bQap `matrixadd` cQap
    ptsMG1       = map (polycommit curveOrder ptsG1) $ transpose dQap
    dPt''        = polyevalEnc curveOrder witnessGf ptsMG1

prop_checkTpoints s = take 100 tPts == take 100 tPts'
    where
    secret       = abs s
    (ptsG1, _)   = KZG.trustedSetup secret g1 g2
    zSecretEval  = polyevalGf curveOrder gammaG2 secret
    -- compute secret powers first, scale them, and multiply by point g1
    secretPowers = toGf curveOrder $ ModularArithmetic.getPowerCycle secret curveOrder
    tPts         = map (g1 `mul`) $ polyscale zSecretEval secretPowers
    -- compute secret evaluations by the secret evaluation of gammaG2
    tPts'        = map (`mul` zSecretEval) ptsG1

prop_checkTpointsPoly s = tPt == tPt'
    where
    secret       = 2 + abs s
    (ptsG1, _)   = KZG.trustedSetup secret g1 g2
    -- computing a polynomial first, and then evaluating over points
    (_, _, _, t) = computePolysGf curveOrder (aQap, bQap, cQap) witnessGf
    tPt          = polyevalEnc curveOrder t ptsG1
    -- computing points first and then evaluating over q
    zSecretEval  = polyevalGf curveOrder gammaG2 secret
    tPts         = map (`mul` zSecretEval) ptsG1
    q            = fst $ t `polydiv` gammaG2
    tPt'         = polyevalEnc curveOrder q tPts

exampleProveVerify =
        Groth16.verify (curveOrder, alphaG1, betaG2, gammaG2, deltaG2, cPts, pairing) proof (take pubLen Groth16Test.witness)
    where
    secret = 67
    alpha  = 2
    beta   = 2
    gamma  = 2
    delta  = 2
    rSec   = 2
    sSec   = 2
    (ptsG1, ptsG2, alphaG1, betaG1, betaG2, gammaG2, deltaG1, deltaG2, cPts, tPts) =
        Groth16.trustedSetup curveOrder secret g1 g2 alpha beta gamma delta pubLen qap
    -- no public inputs
    pubLen = 0
    proof  = Groth16.prove (curveOrder, ptsG1, ptsG2, alphaG1, betaG1, betaG2, deltaG1, deltaG2, cPts, tPts) qap (Groth16Test.witness, pubLen) rSec sSec

prop_proveVerify s a b g d l rVal sVal =
        Groth16.verify (curveOrder, alphaG1, betaG2, gammaG2, deltaG2, cPts, pairing) proof (take pubLen Groth16Test.witness)
    where
    secret = 2 + abs s
    alpha  = abs a
    beta   = abs b
    gamma  = 1 + abs g
    delta  = 1 + abs d
    rSec   = abs rVal
    sSec   = abs sVal
    pubLen = abs l `mod` (1 + length Groth16Test.witness)
    (ptsG1, ptsG2, alphaG1, betaG1, betaG2, gammaG2, deltaG1, deltaG2, cPts, tPts) =
        Groth16.trustedSetup curveOrder secret g1 g2 alpha beta gamma delta pubLen qap
    proof  = Groth16.prove (curveOrder, ptsG1, ptsG2, alphaG1, betaG1, betaG2, deltaG1, deltaG2, cPts, tPts) qap (Groth16Test.witness, pubLen) rSec sSec

return []
runTests = $quickCheckAll
