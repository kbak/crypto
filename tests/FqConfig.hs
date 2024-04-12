module FqConfig where

-- bn128 (used in Ethereum)
fieldModulus = 21888242871839275222246405745257275088696311157297823662689037894645226208583
-- polynomial: w^2 + 1
fq2Modulus = [1, 0, 1]
-- polynomial: w^12 - 18 * w^6 + 82
fq12Modulus = [82, 0, 0, 0, 0, 0, -18, 0, 0, 0, 0, 0, 1]