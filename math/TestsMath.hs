module TestsMath where

import ModularArithmeticTest
import GfTest
import PolynomialTest
import ModularPolynomialTest
import FFTTest

run = do
    ModularArithmeticTest.runTests
    GfTest.runTests
    PolynomialTest.runTests
    ModularPolynomialTest.runTests
    FFTTest.runTests