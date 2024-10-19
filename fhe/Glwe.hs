-- based on:
-- https://www.zama.ai/post/tfhe-deep-dive-part-1
-- https://www.zama.ai/post/tfhe-deep-dive-part-2
module GLWE where

import Basic
import GCf
import Polynomial
import System.Random
import Data.Random.Normal
import Control.Monad

-- cyclomatic polynomial (1 + x^n)
cPolynomial 0 = [1]
cPolynomial n = (1 : replicate (n - 1) 0) ++ [1]

-- converts a list of integrals to GCf
toGCf p = map ((`intGCf` p).fromIntegral)

-- multiplies polynomials x and y and computes modulo m
polymultmod m x y = snd $ (x `polymult` y) `polydiv` m

-- multiplies polynomials x and y and computes modulo the cyclomatic polynomial (1 + x^n)
polymultcyclo q n = polymultmod cycloPoly
  where
  -- centered/normalized cyclomatic polynomial
  cycloPoly = toGCf q $ cPolynomial n
  
-- A_0 * S_0 + ... A_(k-1) * S_(k-1) 
asSum q n s a = foldl1 polyadd ms
  where
  -- center/normalize the key and the mask
  cs = map (toGCf q) s
  cm = map (toGCf q) a
  -- [A_0 * S_0, ...,A_(k-1) * S_(k-1)]
  ms = zipWith (polymultcyclo q n) cm cs

-- GLWE encryption
encryptGLWE (p, q, n) s a e m
  | p > q                = error "incorrect p and q values"
  | length s /= length a = error "wrong lengths of secret key and mask"
  | otherwise            = b
  where
  -- delta = q / p
  delta  = q `div` p
  -- centered/normalized delta * message
  deltaM = polyscale (intGCf delta q) (toGCf q m)
  msSum  = asSum q n s a
  -- center/nomalize Gaussian error
  ce = toGCf q e
  -- body = A_0 * S_0 + ... A_(k-1) * S_(k-1) + deltaM + e
  b  = msSum `polyadd` deltaM `polyadd` ce

decryptGLWE (p, q, n) (a, b) s = toGCf p dmeByDelta
  where
  delta = fromIntegral $ q `div` p
  msSum = asSum q n s a
  dme   = map gcf $ b `polysub` msSum
  dmeByDelta = map (\f -> round $ fromIntegral f / delta) dme

encryptLWE (p, q) = encryptGLWE (p, q, 1)

decryptLWE (p, q) = decryptGLWE (p, q, 1)

-- generates a polynomial of degree (n-1) with binary elements (mod 2)
-- uses uniform distribution
genPolyBinary n = do
  gen <- newStdGen
  let randomNumbers = randomRs (0, 1) gen :: [Int]
  return $ take n randomNumbers

-- generates a polynomial of degree (n-1) with elements mod m centered around 0
-- uses uniform distribution
genPolyUniform n m = do
  gen <- newStdGen
  let halfM = m `div` 2
  let lo = -halfM
  let hi = halfM - (if even m then 1 else 0)
  let randomNumbers = randomRs (lo, hi) gen :: [Int]
  return $ take n randomNumbers

-- generates a polynomial of degree (n-1) with elements mod m centered around 0
-- uses normal distribution. Absolute value of each coefficient < delta / 2
genPolyGauss n delta = do
  gen <- newStdGen
  let randomNumbers  = normals gen :: [Double]
  let randomIntegers = map round randomNumbers
  let ce             = map gcf $ toGCf (delta - 1) randomIntegers
  return $ take n ce

-- generates a secret key which is composed of k binary polynomials of degree n-1
genSecretKey n k = do
  replicateM k (genPolyBinary n)