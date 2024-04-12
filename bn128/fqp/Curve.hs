module Curve where

import FqConfig
import Field
import Fq
import Fqp
import EllipticCurves

-- BN128 curve order
curveOrder = 21888242871839275222246405745257275088548364400416034343698204186575808495617

-- curve is y^2 = x^3 + 3
b = Fq 3
-- twisted curve over FQ2 y^2 = x^3 + 3 / (9 + i)
b2 = fqp2 [3, 0] / fqp2 [9, 1]

-- generator for curve over FQ
g1 = Point (Fq 1) (Fq 2)

-- generator for twisted curve over FQ2
g2 = Point (fqp2 [ 10857046999023057135944570762232829481370756359578518086990519993285655852781
                , 11559732032986387107991004021392285783925812861821192530917403151452391805634 ])
           (fqp2 [8495653923123431417604973247489272438418190587263600148770280649306958101930
                , 4082367875863433681332203403145435568316851327593401208105741076214120093531 ])

twist O = O
twist (Point (Fqp [x0, x1] _) (Fqp [y0, y1] _)) = Point x y
  where
  -- field isomorphism from Z[p] / x^2 to Z[p] / x^2 - 18*x + 82
  x0' = toInteger $ x0 - (x1 * 9)
  x1' = toInteger x1
  y0' = toInteger $ y0 - (y1 * 9)
  y1' = toInteger y1
  zeros5 = replicate 5 0
  -- isomorphism into subfield of Z[p] / w^12 - 18 * w^6 + 82, where w^6 = x
  nx = fqp12 $ [x0'] ++ zeros5 ++ [x1'] ++ zeros5
  ny = fqp12 $ [y0'] ++ zeros5 ++ [y1'] ++ zeros5
  -- divide x coord by w^2 and y coord by w^3
  x  = nx * (w <^> (2 :: Int))
  y  = ny * (w <^> (3 :: Int))
  -- "Twist" a point in E(FQ2) into a point in E(FQ12)
  w = fqp12 [0, 1]

pairing = pairingGeneric fieldModulus curveOrder g2 29793968203157093288 b b2 twist postProcessMiller

postProcessMiller q@(Point x0 y0) p (f, r) = f1 * lineFunc (r `add` q1) nq2 p
    where
    q1@(Point x1 y1) = Point (x0 <^> fieldModulus) (y0 <^> fieldModulus)
    nq2              = Point (x1 <^> fieldModulus) ((-y1) <^> fieldModulus)
    f1               = f * lineFunc r q1 p
