{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll

module EllipticCurvesTest where

-- some code implemented following https://www.rareskills.io/post/elliptic-curves-finite-fields
-- and https://www.rareskills.io/post/bilinear-pairing

import Gf
import EllipticCurves
import Test.QuickCheck
import Data.Maybe

curveOrder = 52435875175126190479447740508185965837690552500527637822603658699938581184513

-- creates an element of Z_p where p is the curve order
intZp n = intGf n curveOrder

-- creates a point on EC y^2 = x^3 + 1 from a given x
mkPoint x = points (intZp 0, intZp 1) (intZp x)

-- creates a pair of points from x on a given EC y^2 = x^3 + ax + b
points curve x = Gf.sqrt (evalPoint curve x) >>= (\(y1, y2) -> Just (Point x y1, Point x y2))
  
-- p1 + p2 == p2 + p1
prop_addPoints x1 x2 = (isJust mp1 && isJust mp2) ==>
  (p1 `add` p2) == (p2 `add` p1)
  where
  mp1 = mkPoint x1
  mp2 = mkPoint x2
  p1 = fst $ fromJust mp1
  p2 = fst $ fromJust mp2

-- 2*p = p + p
prop_doublePoint x y = double p == add p p
  where
  p = Point (intZp x) (intZp y)

-- 2*2p == p+p+p+p
prop_quadruple x = isJust mp ==> (double.double) p ==
  (p `add` p `add` p `add` p)
  where
  mp = mkPoint x
  p = fst $ fromJust mp

return []
runTests = $quickCheckAll