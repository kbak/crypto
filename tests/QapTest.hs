{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll

module QAPTest where

import Data.Map
import Data.Maybe
import Data.List
import Basic
import Polynomial
import QAP
import Fq
import Gf
import Test.QuickCheck

-- test code that relies on Num a type for values in the AST

{-
    Flat code for:
    ```
    def qeval(x):
        y = x**3
        return x + y + 5
    ```

    where x is the private input
-}
flatCode :: Num a => [Statement a]
flatCode = [
    Statement "sym_1" $ Operation (Variable "x") Mul (Variable "x"), -- sym_1 = x * x
    Statement "y" $ Operation (Variable "sym_1") Mul (Variable "x"), -- y = sym_1 * x
    Statement "sym_2" $ Operation (Variable "y") Add (Variable "x"), -- sym_2 = y + x
    Statement "~out" $ Operation (Variable "sym_2") Add (Value 5)    -- ~out = sym_2 + 5
    ]

-- variables
variables = ["~one", "x", "~out", "sym_1", "y", "sym_2"]

-- initial variable assignments
assignment :: Num a => Map String a
assignment = fromList [("~one", 1), ("x", 3)]

-- ordered assignment of values to `variables`
witness :: Num a => [a]
witness = [1, 3, 35, 9, 27, 30]

prop_witness = witness == getWitness variables assignment flatCode

r1csGates :: Num a => [([a], [a], [a])]
r1csGates = [
    -- gate 1 (A, B, C)
    (   [0, 1, 0, 0, 0, 0],
        [0, 1, 0, 0, 0, 0],
        [0, 0, 0, 1, 0, 0]
    ),
    -- gate 2 (A, B, C)
    (   [0, 0, 0, 1, 0, 0],
        [0, 1, 0, 0, 0, 0],
        [0, 0, 0, 0, 1, 0]
    ),
    -- gate 3 (A, B, C)
    (   [0, 1, 0, 0, 1, 0],
        [1, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 1]
    ),
    -- gate 4 (A, B, C)
    (   [5, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 0, 0],
        [0, 0, 1, 0, 0, 0]
    )
    ]

prop_codeToR1cs = gatesToR1cs r1csGates == codeToR1cs variables flatCode

-- checking that the solution satisfies all the constrants
prop_isR1csGates = all (isR1cs witness) r1csGates

isPolynomialEmpty p = not (any (/= 0) p)

zPolynomial :: Num a => [a]
zPolynomial = [24, -50, 35, -10, 1]

-- test code that uses floating point numbers for values in the AST

-- polynomials after rounding their coefficients (only for testing)
polynomialsFloat = (
    -- matrix A
    Data.List.transpose
    [   [-5.0, 9.167, -5.0, 0.833],
        [8.0, -11.333, 5.0, -0.667],
        [0.0, 0.0, 0.0, 0.0],
        [-6.0, 9.5, -4.0, 0.5],
        [4.0, -7.0, 3.5, -0.5],
        [-1.0, 1.833, -1.0, 0.167]
    ],
    -- matrix B
    Data.List.transpose
    [   [3.0, -5.167, 2.5, -0.333],
        [-2.0, 5.167, -2.5, 0.333],
        [0.0, 0.0, 0.0, 0.0],
        [0.0, 0.0, 0.0, 0.0],
        [0.0, 0.0, 0.0, 0.0],
        [0.0, 0.0, 0.0, 0.0]
    ],
    -- matrix C
    Data.List.transpose
    [   [0.0, 0.0, 0.0, 0.0],
        [0.0, 0.0, 0.0, 0.0],
        [-1.0, 1.833, -1.0, 0.167],
        [4.0, -4.333, 1.5, -0.167],
        [-6.0, 9.5, -4.0, 0.5],
        [4.0, -7.0, 3.5, -0.5]
    ]
    )

-- polynomials dot products after rounding their coefficients (only for testing)
solutionPolynomialsFloat = (
    -- A . s
    [43.0, -73.333, 38.5, -5.167],
    -- B . s
    [-3.0, 10.333, -5.0, 0.667],
    -- C . s
    [-41.0, 71.667, -24.5, 2.833]
    )

-- t polynomial after rounding its coefficients (only for testing)
tPolynomialFloat = [-88.0, 592.667, -1063.778, 805.833,-294.778, 51.5, -3.444]

-- h = t / Z polynomial after rounding its coefficients (only for testing)
hPolynomialFloat = [-3.667,17.056,-3.444]

-- a verifier would check the solution s on QAP matrices by computing the polynomial t(s)
-- and then verifying that it divides by polynomial Z cleanly. The polynomial Z represents
-- gates 1,2,3,4... by having zeros at these x's. So if t(s) divides by Z cleanly, it also
-- must have the same zeros.
isQapSolutionFloat (a, b, c, z) s = isPolynomialEmpty $ roundVector3 r
    where
    (_, _, _, t) = computePolys (a, b, c) s
    r            = snd $ polydiv t z

roundVector3 = Prelude.map (roundFrac 3)

roundMatrix3 = Prelude.map roundVector3

roundVector = Prelude.map (\x -> round x :: Integer)

roundMatrix = Prelude.map roundVector

prop_codeToQapFloat = zPolynomial == z &&
    polynomialsFloat == (roundMatrix3 a, roundMatrix3 b, roundMatrix3 c) &&
    solutionPolynomialsFloat == (roundVector3 as, roundVector3 bs, roundVector3 cs) &&
    isQapSolutionFloat qap witness &&
    tPolynomialFloat == roundVector3 t &&
    hPolynomialFloat == roundVector3 q
    where
    r1cs             = codeToR1cs variables flatCode
    qap@(a, b, c, z) = codeToQap variables flatCode
    (as, bs, cs, t)  = computePolys (a, b, c) witness
    (q, _)           = polydiv t z

prop_qapToR1csFloat =
    a == roundMatrix (qapMatrixToR1cs aQap) &&
    b == roundMatrix (qapMatrixToR1cs bQap) &&
    c == roundMatrix (qapMatrixToR1cs cQap)
    where
    (a, b, c) = codeToR1cs variables flatCode
    (aQap, bQap, cQap, _) = codeToQap variables flatCode

-- test code that uses Fq prime field numbers for values in the AST

prop_witnessFq = (witness :: [Fq]) == getWitness variables assignment flatCode

prop_codeToR1csFq = gatesToR1cs r1csGates == codeToR1cs variables (flatCode :: [Statement Fq])

-- checking that the solution satisfies all the constrants
prop_isR1csGatesFq = all (isR1cs (witness :: [Fq])) r1csGates

isQapSolutionFq (a, b, c, z) s = isPolynomialEmpty r
    where
    -- t is computed by the prover
    (_, _, _, t) = computePolys (a, b, c) s
    r            = snd $ polydiv t z

prop_codeToQapFq = zPolynomial == z &&
    isQapSolutionFq qap witness
    where
    -- witness is computed by the prover, then it needs to make it succinct
    -- verifier computes QAP and checks on the succinct witness
    qap@(_, _, _, z) = codeToQap variables (flatCode :: [Statement Fq])

prop_qapToR1csFq =
    a == qapMatrixToR1cs aQap &&
    b == qapMatrixToR1cs bQap &&
    c == qapMatrixToR1cs cQap
    where
    (a, b, c) = codeToR1cs variables (flatCode :: [Statement Fq])
    (aQap, bQap, cQap, _) = codeToQap variables flatCode

-- test code that uses Gf prime field numbers for values in the AST

modulus = 21888242871839275222246405745257275088548364400416034343698204186575808495617
intZp n = intGf n modulus
oneZp   = intZp 1

r1csMatrixToGf = Prelude.map (Prelude.map intZp)

r1csGfMatrixToQap m = Prelude.map (lagrangeGeneric oneZp (Prelude.map intZp [1..])) (Data.List.transpose m)

test_codeToR1csGF = (aQap, bQap, cQap, z)
    where
    (a, b, c) = codeToR1cs variables (flatCode :: [Statement Integer])
    aQap      = r1csGfMatrixToQap $ r1csMatrixToGf a
    bQap      = r1csGfMatrixToQap $ r1csMatrixToGf b
    cQap      = r1csGfMatrixToQap $ r1csMatrixToGf c
    z         = zPolyGeneric oneZp $ Prelude.map intZp [1 .. fromIntegral $ length a]

return []
runTests = $quickCheckAll