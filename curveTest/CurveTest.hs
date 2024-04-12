{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll

-- some tests are from https://www.rareskills.io/post/bilinear-pairing

module CurveTest where

import ModularArithmetic
import FqConfig
import Field
import Curve
import EllipticCurves
import Test.QuickCheck

g12     = twist g2
oneFq12 = toOne (x g12)

-- g + g == dDouble
prop_addG1 = g1 `add` g1 == g1 `mul` 2

-- g + g == 2g
prop_addG2 = g2 `add` g2 == g2 `mul` 2

-- 2*g == 2g
prop_mulG2 = g2 `mul` 2 == double g2

-- g + g + g == 3g
prop_tripleG1 = g1 `add` g1 `add` g1 == mul g1 3

-- 10G + 11G = 21G
prop_g21 = (g1 `mul` 10) `add` (g1 `mul` 11) == g1 `mul` 21

-- basic ZK: “I know two values x and y such that x + y = 15”
-- Although the verifier doesn’t know what x and y are,
-- they can verify that x and y add up to 15 in elliptic curve space
prop_basicZK = x `add` y == g1 `mul` 15
  where
  -- proof = (x, y, 15)
  secretX = 5
  secretY = 10
  x = g1 `mul` secretX
  y = g1 `mul` secretY

-- curve order should be prime
prop_curvePrime = pow 2 curveOrder curveOrder == 2

-- curve order should be a factor of field_modulus**12 - 1
prop_curveFactor = (fieldModulus^12 - 1) `mod` curveOrder == 0

-- 2*g + g + g == 2*2*g
prop_doubleAddG1 = (double g1 `add` g1) `add` g1 == (double.double) g1
prop_doubleAddG2 = (double g2 `add` g2) `add` g2 == (double.double) g2
prop_doubleAddG12 = (double g12 `add` g12) `add` g12 == (double.double) g12

-- 2*g /= g
prop_notDoubleG1 = double g1 /= g1
prop_notDoubleG2 = double g2 /= g2
prop_notDoubleG12 = double g12 /= g12

-- 9*g + 5*g == 12*g + 2*g
prop_mulAddG1 = (g1 `mul` 9) `add` (g1 `mul` 5) == (g1 `mul` 12) `add` (g1 `mul` 2)
prop_mulAddG2 = (g2 `mul` 9) `add` (g2 `mul` 5) == (g2 `mul` 12) `add` (g2 `mul` 2)
prop_mulAddG12 = (g12 `mul` 9) `add` (g12 `mul` 5) == (g12 `mul` 12) `add` (g12 `mul` 2)

-- g * curveOrder == O
prop_isOG1 = g1 `mul` curveOrder == O
prop_isOG2 = g2 `mul` curveOrder == O
prop_isOG12 = g12 `mul` curveOrder == O

-- g belongs to BN128
prop_onCurveG1  = isOnEC (0, b) g1
prop_onCurveG2  = isOnEC (0, b2) (g2 `mul` 9)
prop_onCurveG12 = isOnEC (0, b12) g12
  where
  b12 = fromInteger (toInteger b) * oneFq12

-- g * (2 * fieldModulus - ) /= O
prop_notInf = g2 `mul` (2 * fieldModulus - curveOrder) /= O

-- check consistency of the "line function"
one      = g1
two      = double g1
three    = g1 `mul` 3
negone   = g1 `mul` (curveOrder - 1)
negtwo   = g1 `mul` (curveOrder - 2)
negthree = g1 `mul` (curveOrder - 3)

prop_lineFunc0 = lineFunc one two one == 0
prop_lineFunc1 = lineFunc one two two == 0
prop_lineFunc2 = lineFunc one two three /= 0
prop_lineFunc3 = lineFunc one two negthree == 0
prop_lineFunc4 = lineFunc one negone one == 0
prop_lineFunc5 = lineFunc one negone negone == 0
prop_lineFunc6 = lineFunc one negone two /= 0
prop_lineFunc7 = lineFunc one one one == 0
prop_lineFunc8 = lineFunc one one two /= 0
prop_lineFunc9 = lineFunc one one negtwo == 0

-- pairing tests
p1  = pairing g2 g1
pn1 = pairing g2 (neg g1)
prop_pairingNegativeG1 = p1 * pn1 == oneFq12

np1 = pairing (neg g2) g1
prop_pairingNegativeG2 = p1 * np1 == oneFq12 && pn1 == np1

prop_pairingOutputOrder = p1 <^> curveOrder == oneFq12

p2 = pairing g2 (g1 `mul` 2)
prop_pairingBilinearityG1 = p1 * p1 == p2

prop_pairingNonDegenerate = p1 /= p2 && p1 /= np1 && p2 /= np1

po2 = pairing (g2 `mul` 2) g1
prop_pairingBilinearityG2 = p1 * p1 == po2

p3  = pairing (g2 `mul` 27) (g1 `mul` 37)
po3 = pairing g2 (g1 `mul` 999)
prop_composite = p3 == po3

bp1 = pairing (g2 `mul` 5) (g1 `mul` 6)

prop_bp2 = bp1 == pairing (g2 `mul` (5 * 6)) g1

prop_bp3 = bp1 == pairing g2 (g1 `mul` (5 * 6))

-- see https://eips.ethereum.org/EIPS/eip-197
-- https://www.rareskills.io/post/bilinear-pairing is somewhat confusing
-- since it talks about addition and comparing to zero (it works if we log elements)
prop_eip197 = -a * b + c * d == 0 && e1 * e2 == oneFq12
    where
    a = 4
    b = 3
    c = 6
    d = 2
    aG1 = neg (g1 `mul` a)
    bG2 = g2 `mul` b
    cG1 = g1 `mul` c
    dG2 = g2 `mul` d
    e1  = pairing bG2 aG1
    e2  = pairing dG2 cG1

-- see https://hackmd.io/@liangcc/bls-solidity#alt_bn128-bn256-bn254-why-so-many-names-and-so-confusing
-- signature verification for a given message and public key that corresponds
-- to the private key that created the signature
prop_sigVerify = e1 * e2 == oneFq12
    where
    msg     = 4
    privKey = 5
    sig     = g1 `mul` (msg * privKey)
    pubKey  = g2 `mul` privKey
    e1      = pairing (neg g2) sig
    e2      = pairing pubKey (g1 `mul` msg)

return []
runTests = $quickCheckAll