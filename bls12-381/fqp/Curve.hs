-- based on https://github.com/ethereum/py_ecc/blob/master/py_ecc/bls12_381/bls12_381_pairing.py
module Curve where

import FqConfig
import Field
import Fq
import Fqp
import EllipticCurves

-- BLS12 381 curve order
curveOrder = 52435875175126190479447740508185965837690552500527637822603658699938581184513

-- curve is y^2 = x^3 + 4
b = Fq 4
-- twisted curve over FQ2: y^2 = x^3 + 4(1 + i)
b2 = fqp2 [4, 4]

-- generator for curve over FQ
g1 = Point (Fq 3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507)
           (Fq 1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569)

-- generator for twisted curve over FQ2
g2 = Point (fqp2 [ 352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160
                , 3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758 ])
           (fqp2 [1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905
                , 927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582 ])

twist O = O
twist (Point (Fqp [x0, x1] _) (Fqp [y0, y1] _)) = Point x y
  where
  -- field isomorphism from Z[p] / x^2 to Z[p] / x^2 - 2*x + 2
  x0' = toInteger $ x0 - x1
  x1' = toInteger x1
  y0' = toInteger $ y0 - y1
  y1' = toInteger y1
  zeros5 = replicate 5 0
  -- isomorphism into subfield of Z[p] / w^12 - 2 * w^6 + 2, where w^6 = x
  nx = fqp12 $ [x0'] ++ zeros5 ++ [x1'] ++ zeros5
  ny = fqp12 $ [y0'] ++ zeros5 ++ [y1'] ++ zeros5
  -- divide x coord by w^2 and y coord by w^3
  x  = nx / (w <^> (2 :: Int))
  y  = ny / (w <^> (3 :: Int))
  -- "Twist" a point in E(FQ2) into a point in E(FQ12)
  w = fqp12 [0, 1]

pairing = pairingGeneric fieldModulus curveOrder g2 15132376222941642752 b b2 twist (\_ _ (f, _) -> f)
