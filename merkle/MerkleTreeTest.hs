{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll

module MerkleTreeTest where

import Blake
import MerkleTree
import Data.Array
import Data.Bits
import Test.QuickCheck

prop_intBytesConversion n = n >= 0 ==> n == bytesToInt (intToBytes 32 n)

inputs4 = map Right [1, 2, 3, 4]

expectedMerkle = [ ""
                 , "ae984b1b44402f787f2cbd23e11948592b7d7b3947458aaa1af25575e32f79"
                 , "1969df2b9452e8cb44ef8723c82489e8e5dbd696833da1611150c561b48d19"
                 , "2f48f9fae2abaed6961b11aab0dd265d908281882183f7d9dbcfaf5b0c32072"
                 , "00000000000000000000000000000001"
                 , "00000000000000000000000000000002"
                 , "00000000000000000000000000000003"
                 , "00000000000000000000000000000004" ]

prop_merkelize = expectedMerkle == showMerkle (merkelize inputs4)

expectedProof = [ "00000000000000000000000000000004"
                , "00000000000000000000000000000003"
                , "1969df2b9452e8cb44ef8723c82489e8e5dbd696833da1611150c561b48d19" ]

prop_mkBranch = expectedProof == showMerkle (mkBranch (merkelizeA inputs4) (3 :: Int))

prop_verifyBranch = last inputs4 == verifyBranch root 3 (mkBranch mTree (3 :: Int)) True
    where
    mTree = merkelizeA inputs4
    root  = mTree!1

prop_merkleTree = (Right 59) == verifyBranch (t!1) 59 b True
    where
    t = merkelizeA $ map Right [0 .. 127]
    b = mkBranch t (59 :: Int)

inputs16 = [0..15]

test_tree = merkelizeA $ map Right inputs16

getIndices n = [j | j <- map fromInteger inputs16, odd ((n :: Integer) `shiftR` j)]

expectedProofs1 =  [ [ "00000000000000000000000000000000"
                     , "00000000000000000000000000000001"
                     , "18dcc70f571202b5ae85655ebdc2aaac64b1f0fae012f6e84a7f1f2969dc18"
                     , "f19f4be3c10c6be8bae0efb960c22f48e79bd3325dc347b5b4795d01525e6"
                     , "c5e09b26ad6222fc9fab8cef3ab1ccf3f749ba7f9e62b7b4ee2798f4def3c93" ] ]

prop_mkMultiBranch1 = expectedProofs1 == map showMerkle (mkMultiBranch test_tree (getIndices 1))

-- compressed proof. Some nodes are elided because they can be derived
expectedProofs10 = [ [ "00000000000000000000000000000001"
                     , "00000000000000000000000000000000"
                     , ""
                     , "f19f4be3c10c6be8bae0efb960c22f48e79bd3325dc347b5b4795d01525e6"
                     , "c5e09b26ad6222fc9fab8cef3ab1ccf3f749ba7f9e62b7b4ee2798f4def3c93"
                     ],
                     [ "00000000000000000000000000000003"
                     , "00000000000000000000000000000002"
                     , ""
                     , ""
                     , "" ] ]

prop_mkMultiBranch10 = expectedProofs10 == map showMerkle (mkMultiBranch test_tree (getIndices 10))

prop_bitLength = 324 == binLength proofs' && 196 == binLength proofs
    where
    indices = getIndices 10
    proofs  = mkMultiBranch test_tree indices
    proofs' = map (mkBranch test_tree) indices

verifyMultiMerkleTree i = verifyMultiBranch (test_tree!1) ind branch == [Left $ test_tree!(16 + j) | j <- ind]
    where
    ind    = getIndices i
    branch = mkMultiBranch test_tree ind

prop_multiMerkleTree1 = verifyMultiMerkleTree 1

prop_multiMerkleTree10 = verifyMultiMerkleTree 10

prop_multiMerkleTree n = withMaxSuccess 1000 $ verifyMultiMerkleTree (abs n `mod` 65535)

return []
runTests = $quickCheckAll