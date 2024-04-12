-- see Vitalik's posts
-- https://dev.risczero.com/proof-system/stark-by-hand (SbH)
-- https://starkware.co/stark-101/ (S101)
module MIMCStark where

import Basic
import Field
import FqConfig
import ModularArithmetic
import Fq
import Polynomial
import Blake
import MerkleTree
import FFT
import FRI
import Data.Array
import Data.Either
import Data.List
import qualified Data.ByteString as B

spotCheckSecurityFactor = 80
extensionFactor         = 8
intExtensionFactor      = fromIntegral extensionFactor

-- generate a STARK for a MIMC calculation
mkMIMCProof inp steps roundConstants
    | steps > 2^32 `div` extensionFactor     = error "Too many steps"
    | not $ isPower2 steps                   = error "Steps not a power of 2"
    | not $ isPower2 $ length roundConstants = error "Round constants not a power of 2"
    | length roundConstants >= steps         = error "Too many round constants"
    | otherwise                              = o
    where
    precision    = steps * extensionFactor
    intPrecision = fromIntegral precision
    -- root of unity such that x^precision=1
    g2 = pow 7 ((fieldModulus - 1) `div` intPrecision) fieldModulus
    -- root of unity such that x^steps=1 (because precision = steps * extensionFactor)
    g1 = pow g2 extensionFactor fieldModulus
    -- powers of the higher-order root of unity
    xs   = getPowerCycle g2 fieldModulus
    xsFq = map Fq xs
    lastStepPosition = xs!!((steps - 1) * extensionFactor)
    -- generate the computational trace
    computationalTrace = mkComputationalTrace 0 inp
    output             = last computationalTrace
    -- interpolate the computational trace into a polynomial P, with each step
    -- along a successive power of G1. See SbH, lesson 4. The inverse FFT constructs
    -- a polynomial in the coefficient form based on a set of points (g1^i, trace_i)
    -- see S101, lesson 1 (input: computational trace)
    computationalTracePolynomial = fftMain computationalTrace fieldModulus g1 True
    -- evaluations (points) in terms of g2, i.e., (g2^i, trace'_i) give a Reed-Salomon encoded trace
    -- evaluations of g2 provide an expanded domain and provide redundancy for error checking. Here
    -- we have 1/extensionFactor RS encoding so for each item in the original g1 domain, we have 7
    -- extra items in the expanded g2 domain. In other words `pEvaluations` is 8x longer than
    -- `computationalTrace`. See:
    -- https://docs.google.com/spreadsheets/d/1Onr41OozD62y-B0jIL7bHAH5kf771-o4xvmnHUFpOyo/edit#gid=690777158
    -- see S101, lesson 1 (regarding the much larger domain that can recover the polynomial that
    -- interpolates the computational trace)
    pEvaluations            = fftMain computationalTracePolynomial fieldModulus g2 False
    skips2                  = steps `div` length roundConstants
    -- do the same Reed-Salomon encoding as above but with `roundConstants`
    constantsMiniPolynomial = fftMain (elems roundConstants) fieldModulus (pow g1 skips2 fieldModulus) True
    -- the following is going to be our K(x)
    constantsMiniExtension  = fftMain constantsMiniPolynomial fieldModulus (pow g2 skips2 fieldModulus) False
    -- create the composed (mixed constraint) polynomial such that
    -- C(P(x), P(g1*x), K(x)) = P(g1*x) - P(x)**3 - K(x) (SbH lesson 7)
    -- a single mixed constraint polynomial is used in FRI
    cOfPEvaluations = zipWith3 (\pEval pEvalC cme ->
                                (pEvalC - pow pEval (3 :: Integer) fieldModulus - cme) `mod` fieldModulus)
                        pEvaluations (cycleL extensionFactor pEvaluations) (cycle constantsMiniExtension)
    indices          = 0 : takeWhile (/= 0) (map (\i -> (i * steps) `mod` precision) [1..])
    -- Z(x) = (x^steps - 1) / (x - x_atlast_step). First, calculate 1 / numerator(Z(x)) (SbH lesson 8)
    zNumInv          = cycle $ ModularArithmetic.multiInv [(xs!!i) - 1 | i <- indices] fieldModulus
    -- compute D(x) = C((P(x), P(g1*x), K(x)) / Z(x). First calculate D(x) = C(P(x)) / Z(x)
    dEvaluations     = zipWith3 (\cp x zni -> cp * (x - lastStepPosition) * zni `mod` fieldModulus) cOfPEvaluations xs zNumInv
    -- compute interpolant of ((1, input), (x_atlast_step, output))
    interpolant      = lagrange2 [1, fromInteger lastStepPosition] [fromInteger inp, fromInteger output]
    iEvaluations     = map (toInteger . polyeval interpolant) xsFq
    zeropoly2        = polymult [-1, 1] [fromInteger $ -lastStepPosition, 1]
    invZ2Evaluations = ModularArithmetic.multiInv (map (toInteger . polyeval zeropoly2) xsFq) fieldModulus
    bEvaluations     = zipWith3 (\p i invq -> ((p - i) * invq) `mod` fieldModulus) pEvaluations iEvaluations invZ2Evaluations
    -- compute their Merkle root
    -- see S101, lesson 1 where pEvaluations (extended domain) get committed to the Merkle tree
    mtree = merkelizeA [Left $ B.concat $ map intToBytes32 [pval, dval, bval] |
                        (pval, dval, bval) <- zip3 pEvaluations dEvaluations bEvaluations]
    -- based on the hashes of P, D and B, we select a random linear combination
    -- of P * x^steps, P, B * x^steps, B and D, and prove the low-degreeness of that,
    -- instead of proving the low-degreeness of P, B and D separately
    [k1, k2, k3, k4] = map (bytesToInt . blake . B.append (mtree!1) . B.singleton) [01, 02, 03, 04]
    -- compute the linear combination. We don't even bother calculating it in
    -- coefficient form; we just compute the evaluations
    g2ToTheSteps = pow g2 steps fieldModulus
    powers       = take precision $ iterate (\x -> (x * g2ToTheSteps) `mod` fieldModulus) 1
    lEvaluations = zipWith4 (\d p b power ->
                        Right $ (d + p * (k1 + k2 * power) + b * (k3 + power * k4)) `mod` fieldModulus)
                    dEvaluations pEvaluations bEvaluations powers
    --  SbH lesson 8
    lMtree       = merkelizeA lEvaluations
    -- do some spot checks of the Merkle tree at pseudo-random coordinates, excluding
    -- multiples of `extensionFactor`
    positions = getPseudorandomIndices (lMtree!1) intPrecision spotCheckSecurityFactor intExtensionFactor
    augmentedPositions = concatMap (\x -> [x, (x + fromIntegral extensionFactor) `mod` intPrecision]) positions
    -- return the Merkle roots of P and D, the spot check Merkle proofs,
    -- and low-degree proofs of P and D
    o = ( mtree!1,
          lMtree!1,
          mkMultiBranch  mtree augmentedPositions,
          mkMultiBranch lMtree positions,
          -- SbH lesson 9, 10
          proveLowDegree lEvaluations g2 (steps * 2) fieldModulus intExtensionFactor )

    -- makes a computational trace for MIMC
    mkComputationalTrace n input
        | n < steps - 1 = input : mkComputationalTrace (n + 1) ((input^3 + roundConstants!(n `mod` length roundConstants)) `mod` fieldModulus)
        | otherwise     = [input]

-- verifies a STARK
verifyMimcProof inp steps roundConstants output proof@(mRoot, lRoot, mainBranches, linearCombBranches, friProof)
    | steps > 2^32 `div` extensionFactor     = error "Too many steps"
    | not $ isPower2 steps                   = error "Steps not a power of 2"
    | not $ isPower2 $ length roundConstants = error "Round constants not a power of 2"
    | length roundConstants >= steps         = error "Too many round constants"
    | not isProofOk                          = error "Incorrect low degree proof"
    | not areConstraintsOk                   = error "Incorrect constraints"
    | otherwise                              = True
    where
    precision    = steps * extensionFactor
    intPrecision = fromIntegral precision
    -- get (steps)th root of unity
    g2    = pow 7 ((fieldModulus - 1) `div` intPrecision) fieldModulus
    -- gets the polynomial representing the round constants
    skips2 = steps `div` length roundConstants
    rootOfUnity = pow g2 (extensionFactor * skips2) fieldModulus
    constantsMiniPolynomial = map fromInteger $ fftMain (elems roundConstants) fieldModulus rootOfUnity True
    -- verifies the low-degree proofs
    isProofOk              = verifyLowDegreeProof lRoot g2 friProof (steps * 2) fieldModulus intExtensionFactor
    [k1, k2, k3, k4]       = map (fromInteger . bytesToInt . blake . B.append mRoot . B.singleton) [01, 02, 03, 04]
    positions              = getPseudorandomIndices lRoot intPrecision spotCheckSecurityFactor intExtensionFactor
    augmentedPositions     = concatMap (\x -> [x, (x + fromIntegral extensionFactor) `mod` intPrecision]) positions
    lastStepPosition       = pow g2 ((steps - 1) * extensionFactor) fieldModulus
    fqLastStepPosition     = fromInteger lastStepPosition
    mainBranchLeaves       = lefts $ verifyMultiBranch mRoot augmentedPositions mainBranches
    linearCombBranchLeaves = lefts $ verifyMultiBranch lRoot positions linearCombBranches
    areConstraintsOk       = all verifyConstraints (zip3 positions linearCombBranchLeaves (chunks 2 mainBranchLeaves))

    verifyConstraints (pos, linearCombBranchLeave, [mbranch1, mbranch2])
        | not areTransConstOk = error "Incorrect transition constraints"
        | not areBoundConstOk = error "Incorrect boundary constraints"
        | not isLinearCombOk  = error "Incorrect linear combination"
        | otherwise           = True
        where
        x               = pow g2 pos fieldModulus
        fqX             = fromInteger x
        -- operations in a finite field
        xToTheSteps     = fromInteger $ pow x steps fieldModulus
        lOfX            = fromInteger $ bytesToInt linearCombBranchLeave
        (pOfXB, dOfXB)  = B.splitAt 32 mbranch1
        (dOfXB', bOfXB) = B.splitAt 32 dOfXB
        pOfX            = fromInteger $ bytesToInt pOfXB
        dOfX            = fromInteger $ bytesToInt dOfXB'
        bOfX            = fromInteger $ bytesToInt bOfXB
        pOfG1x          = fromInteger $ bytesToInt $ B.take 32 mbranch2
        zvalue          = ((fqX <^> steps) - 1) `div` (fqX - fqLastStepPosition)
        kOfX            = polyeval constantsMiniPolynomial (fqX <^> skips2)
        -- check transition constraints C(P(x)) = Z(x) * D(x)
        areTransConstOk = 0 == pOfG1x - (pOfX^3) - kOfX - zvalue * dOfX
        -- check boundary constraints B(x) * Q(x) + I(x) = P(x)
        interpolant     = lagrange2 [1, fqLastStepPosition] [fromInteger inp, fromInteger output]
        zeropoly2       = polymult  [-1 :: Fq, 1] [-fqLastStepPosition, 1]
        areBoundConstOk = 0 == pOfX - bOfX * polyeval zeropoly2 fqX - polyeval interpolant fqX
        -- check correctness of the linear combination
        isLinearCombOk  = 0 == lOfX - dOfX - k1 * pOfX - k2 * pOfX * xToTheSteps - k3 * bOfX - k4 * bOfX * xToTheSteps
