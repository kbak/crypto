{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll
module MIMCStarkTest where

import FqConfig
import MIMC
import MerkleTree
import FRI
import MIMCStark
import Data.Array
import Data.Bits
import Test.QuickCheck

-- full STARK test
prop_stark = verifyMimcProof inp steps constantsA output proof && l1 == 82272 && l2 == 161808
    where
    inp        = 3
    logsteps   = 13
    steps      = 2 ^ logsteps
    nElements  = 64
    constants  = [(fromIntegral i ^ 7) `xor` 42 | i <- [0 .. nElements - 1]]
    constantsA = listArray (0, nElements - 1) constants
    output     = mimc inp steps constantsA fieldModulus
    proof@(_, _, mainBranches, linearCombBranches, friProof) = mkMIMCProof inp steps constantsA
    l1 = binLength mainBranches + binLength linearCombBranches
    l2 = friProofBinLength friProof

return []
runTests = $quickCheckAll