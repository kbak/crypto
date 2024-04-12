module Curve where

import FqConfig
import Field
import Fq
import Fq2
import Fq6
import Fq12
import EllipticCurves

-- BN128 curve order
curveOrder = 21888242871839275222246405745257275088548364400416034343698204186575808495617

-- curve is y^2 = x^3 + 3
b = Fq 3
-- twisted curve over FQ2 y^2 = x^3 + 3 / (9 + i)
b2 = Fq2 3 0 / Fq2 9 1

-- generator for curve over FQ
g1 = Point (Fq 1) (Fq 2)

-- generator for twisted curve over FQ2
g2 = Point (Fq2 10857046999023057135944570762232829481370756359578518086990519993285655852781
                11559732032986387107991004021392285783925812861821192530917403151452391805634)
           (Fq2 8495653923123431417604973247489272438418190587263600148770280649306958101930
                4082367875863433681332203403145435568316851327593401208105741076214120093531)

twist O = O
twist (Point x1 y1) = Point wideX wideY
  where
  root  = Fq6 0 1 0
  wideX = Fq12 (Fq6 x1 0 0) 0 * Fq12 root 0
  wideY = Fq12 (Fq6 y1 0 0) 0 * Fq12 0 root

pairing = pairingGeneric fieldModulus curveOrder g2 29793968203157093288 b b2 twist postProcessMiller

postProcessMiller q@(Point x0 y0) p (f, r) = f1 * lineFunc (r `add` q1) nq2 p
    where
    q1@(Point x1 y1) = Point (x0 <^> fieldModulus) (y0 <^> fieldModulus)
    nq2              = Point (x1 <^> fieldModulus) ((-y1) <^> fieldModulus)
    f1               = f * lineFunc r q1 p
