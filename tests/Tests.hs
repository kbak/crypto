module Tests where

import FqTest
import Fq2Test
import Fq6Test
import Fq12Test
import FqpTest
import QAPTest

run = do
    FqTest.runTests
    Fq2Test.runTests
    Fq6Test.runTests
    Fq12Test.runTests
    FqpTest.runTests
    FqpTest.runFqpTests
    QAPTest.runTests