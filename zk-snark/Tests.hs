module Tests where

import ZkSnarkTest
import Groth16Test

run = do
    ZkSnarkTest.runTests
    Groth16Test.runTests