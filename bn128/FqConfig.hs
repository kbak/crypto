module FqConfig where

-- bn128 (used in Ethereum)
-- https://hackmd.io/@jpw/bn254
-- https://hackmd.io/@liangcc/bls-solidity#alt_bn128-bn256-bn254-why-so-many-names-and-so-confusing
fieldModulus = 21888242871839275222246405745257275088696311157297823662689037894645226208583
-- polynomial: w^2 + 1 (Fq2 is actually complex numbers)
fq2Modulus = [1, 0, 1]
-- polynomial: w^12 - 18 * w^6 + 82, if x = w^6 then we have x^2 - 18x + 82
fq12Modulus = [82, 0, 0, 0, 0, 0, -18, 0, 0, 0, 0, 0, 1]

-- If u signifies the other dimension, for a number a + b*u multiplying by
-- u + 9 (comes from Fp6 construction) gets us au + a + bu^2 + bu and
-- because u^2 = -1, we get (a - b) + (a + b)u
mulNonResFq2 a0 a1 = (9 * a0 - a1, a0 + 9 * a1)

{-
    Tower of extensions
        https://www.ietf.org/archive/id/draft-irtf-cfrg-pairing-friendly-curves-08.html

    GF(p^2)  = GF(p)[u] / (u^2 + 1)
    GF(p^6)  = GF(p^2)[v] / (v^3 - (u + 9))
    GF(p^12) = GF(p^6)[w] / (w^2 - v)
-}