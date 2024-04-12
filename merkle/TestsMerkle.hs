module TestsMerkle where

import MerkleTreeTest
import FRITest

run = do
    MerkleTreeTest.runTests
    FRITest.runTests