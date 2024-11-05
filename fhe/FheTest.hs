{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll
module FHETest where

import FHE
import GCf
import Polynomial
import Test.QuickCheck

-- example from https://www.zama.ai/post/tfhe-deep-dive-part-1
prop_glweEncryptDecrypt =
    n >= length s0 &&
    n >= length s1 &&
    k == length s &&
    n >= length m &&
    n >= length a0 &&
    n >= length a1 &&
    k == length a &&
    -- B = -31 + 5 * X - 21 * X^2 + 30 * X^3
    b == [-31, 5, -21, 30] &&
    -- decrypted message should be the same as the original one
    m == map gcf m'
  where
  p = 4
  q = 64
  -- degree of cyclomatic polynomial x^n + 1
  n = 4
  -- the number of polynomials making up the secret key
  k = 2
  -- secret key
  -- S_0 =  X +  X^2
  s0 = [0, 1, 1, 0]
  -- S_1 = 1 +  X^2 +  X^3
  s1 = [1, 0, 1, 1]
  s  = [s0, s1]
  -- message to encrypt
  -- M = -2 + 1 * X + 0 * X^2 - 1 * X^3 = -2 + X - X^3
  m  = [-2, 1, 0, -1]
  -- mask
  -- A_0 = 17 - 2 * X - 24 * X^2 + 9 * X^3
  a0 = [17, -2, -24, 9]
  a1 = [-14, 0, -1, 21]
  -- A_1 = -14 - X^2 + 21 * X^3
  a  = [a0, a1]
  -- discrete Gaussian error
  -- E = - 1 + 1 * X + 0 * X^2 + 1 * X^3
  e  = [-1, 1, 0, 1]
  -- centered/normalized encrypted message
  bc = encryptGLWE (p, q, n) s a e m
  b  = map gcf bc
  m' = decryptGLWE (p, q, n) (a, bc) s

prop_lweEncryptDecrypt =
    k == length s &&
    n >= length m &&
    k == length a &&
    -- B = 26
    b == [26] &&
    -- decrypted message should be the same as the original one
    m == map gcf m'
  where
  p = 4
  q = 64
  -- degree of cyclomatic polynomial x^n + 1
  n = 1
  -- the number of polynomials making up the secret key
  k = 4
  -- secret key
  s = [0, 1, 1, 0]
  -- message to encrypt
  -- M = 1
  m = [1]
  -- mask
  a = [-25, 12, -3, 7]
  -- discrete Gaussian error
  -- E = 1
  e  = [1]
  bc = encryptLWE (p, q) s a e m
  b  = map gcf bc
  m' = decryptLWE (p, q) (a, bc) s

-- example from https://www.zama.ai/post/tfhe-deep-dive-part-2
prop_glweHomomorphicAddition =
    n >= length s0 &&
    n >= length s1 &&
    k == length s &&
    n >= length m &&
    n >= length m' &&
    n >= length a0 &&
    n >= length a0' &&
    n >= length a1 &&
    n >= length a1' &&
    k == length a &&
    k == length a' &&
    -- B = -31 + 5 * X - 21 * X^2 + 30 * X^3
    b  == [-31, 5, -21, 30] &&
    -- B' = -25 + 12 * X^2 - 12 * X^3
    b' == [-25, 0, 12, -12] &&
    -- A_0(+) = 9 + 13 * X - 21 * X^2 - 21 * X^3
    a0Plus == [9, 13, -21, -21] &&
    -- A_1(+) = 9 - 16 * X + 26 * X^2 + 17 * X^3
    a1Plus == [9, -16, 26, 17] &&
    -- M(+) = -2 - 2 * X + X^2 + X^3
    mPlus == [-2, -2, 1, 1] &&
    -- B(+) = 8 + 5 * X - 9 * X^2 + 18 * X^3
    bPlus == [8, 5, -9, 18] &&
    -- decrypted message should be the same as the derived one
    mPlus == map gcf mPlus'
  where
  p = 4
  q = 64
  -- degree of cyclomatic polynomial x^n + 1
  n = 4
  -- the number of polynomials making up the secret key
  k = 2
  -- secret key
  -- S_0 =  X +  X^2
  s0 = [0, 1, 1, 0]
  -- S_1 = 1 +  X^2 +  X^3
  s1 = [1, 0, 1, 1]
  s  = [s0, s1]
  -- messages to encrypt
  -- M = -2 + 1 * X + 0 * X^2 - 1 * X^3 = -2 + X - X^3
  m  = [-2, 1, 0, -1]
  -- M' = X + X^2 - 2 * X^3
  m' = [0, 1, 1, -2]
  -- masks
  -- A_0 = 17 - 2 * X - 24 * X^2 + 9 * X^3
  a0 = [17, -2, -24, 9]
  -- A_1 = -14 - X^2 + 21 * X^3
  a1 = [-14, 0, -1, 21]
  a  = [a0, a1]
  -- A_0' = -8 + 15 * X + 3 * X^2 - 30 * X^3
  a0' = [-8, 15, 3, -30]
  -- A_1' = 23 - 16 * X + 27 * X^2 - 4 * X^3
  a1' = [23, -16, 27, -4]
  a'  = [a0', a1']
  -- discrete Gaussian errors
  -- E = - 1 + 1 * X + 0 * X^2 + 1 * X^3
  e  = [-1, 1, 0, 1]
  -- E' = X - X^2 - X^3
  e' = [0, 1, -1, -1]
  -- centered/normalized encrypted message
  bc  = encryptGLWE (p, q, n) s a e m
  b   = map gcf bc
  bc' = encryptGLWE (p, q, n) s a' e' m'
  b'  = map gcf bc'
  a0Plus = map gcf $ toGCf q a0 `polyadd` toGCf q a0'
  a1Plus = map gcf $ toGCf q a1 `polyadd` toGCf q a1'
  aPlus  = [a0Plus, a1Plus]
  bcPlus = bc `polyadd` bc'
  bPlus  = map gcf bcPlus
  mPlus  = map gcf $ toGCf p m `polyadd` toGCf p m'
  mPlus' = decryptGLWE (p, q, n) (aPlus, bcPlus) s

-- example from https://www.zama.ai/post/tfhe-deep-dive-part-2
prop_glweHomomorphicMultiplicationByConstant =
    n >= length s0 &&
    n >= length s1 &&
    k == length s &&
    n >= length m &&
    n >= length a0 &&
    n >= length a1 &&
    k == length a &&
    n >= length l &&
    -- B = -31 + 5 * X - 21 * X^2 + 30 * X^3
    b == [-31, 5, -21, 30] &&
    -- l * m = 1 + X + X^2 + X^3
    mMult == [1, 1, 1, 1] &&
    -- l * a0 = -31 + 8 * X - 15 * X^2 + 4 * X^3
    a0Mult == [-31, 8, -15, 4] &&
    -- l * a1 = 16 + 23 * X + 16 * X^2 + 29 * X^3
    a1Mult == [16, 23, 16, 29] &&
    -- l * b = 4 + 20 * X - 7 * X^2 + 19 * X^3
    bMult  == [4, 20, -7, 13] &&
    -- decrypted message should be the same as the original one
    mMult == map gcf mMult'
  where
  p = 4
  q = 64
  -- degree of cyclomatic polynomial x^n + 1
  n = 4
  -- the number of polynomials making up the secret key
  k = 2
  -- secret key
  -- S_0 =  X +  X^2
  s0 = [0, 1, 1, 0]
  -- S_1 = 1 +  X^2 +  X^3
  s1 = [1, 0, 1, 1]
  s  = [s0, s1]
  -- message to encrypt
  -- M = -2 + 1 * X + 0 * X^2 - 1 * X^3 = -2 + X - X^3
  m  = [-2, 1, 0, -1]
  -- mask
  -- A_0 = 17 - 2 * X - 24 * X^2 + 9 * X^3
  a0 = [17, -2, -24, 9]
  a1 = [-14, 0, -1, 21]
  -- A_1 = -14 - X^2 + 21 * X^3
  a  = [a0, a1]
  -- discrete Gaussian error
  -- E = - 1 + 1 * X + 0 * X^2 + 1 * X^3
  e  = [-1, 1, 0, 1]
  -- centered/normalized encrypted message
  bc = encryptGLWE (p, q, n) s a e m
  b  = map gcf bc
  -- constant polynomial l = -1 + 2 * X^2 + X^3
  l  = [-1, 0, 2, 1]
  -- l * m
  mMult  = map gcf $ polymultcyclo p n (toGCf p l) (toGCf p m)
  -- l * a0
  a0Mult = map gcf $ polymultcyclo q n (toGCf q l) (toGCf q a0)
  -- l * a1
  a1Mult = map gcf $ polymultcyclo q n (toGCf q l) (toGCf q a1)
  aMult  = [a0Mult, a1Mult]
  -- l * b
  cbMult = polymultcyclo q n (toGCf q l) (toGCf q b)
  bMult  = map gcf cbMult
  mMult' = decryptGLWE (p, q, n) (aMult, cbMult) s

-- example from https://www.zama.ai/post/tfhe-deep-dive-part-4
prop_switchModulus = [-13,6,-2,4,13] == switchModulus 64 32 [-25, 12, -3, 7, 26]

prop_lweModulusSwitching =
    k == length s &&
    n >= length m &&
    k == length a &&
    -- B = 26
    b == [26] &&
    -- decrypted message should be the same as the original one
    m == map gcf m'
  where
  p = 4
  q = 64
  -- degree of cyclomatic polynomial x^n + 1
  n = 1
  -- the number of polynomials making up the secret key
  k = 4
  -- secret key
  s = [0, 1, 1, 0]
  -- message to encrypt
  -- M = 1
  m = [1]
  -- mask
  a = [-25, 12, -3, 7]
  -- discrete Gaussian error
  -- E = 1
  e  = [1]
  bc = encryptLWE (p, q) s a e m
  b  = map gcf bc
  omega = 32
  a' = switchModulus q omega a
  b' = switchModulusGCf q omega bc
  m' = decryptLWE (p, omega) (a', b') s

return []
runTests = $quickCheckAll
