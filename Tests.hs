module Tests where

import TestsMath
import TestsEc
import TestsMerkle
-- tests that don't depend on any FqConfig
run = do
    TestsMath.run
    TestsEc.run
    TestsMerkle.run