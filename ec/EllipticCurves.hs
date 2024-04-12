module EllipticCurves where

import Field
import Data.Maybe
import Data.Sequence (unfoldl)
import Data.List (unfoldr)
import Data.Bits
import Data.Foldable

data Point a = O | Point { x :: a, y :: a }
  deriving (Eq, Show)

evalPoint (a, b) x = x <^> (3 :: Int) + a * x + b

-- returns true if a point is on the given elliptic curve
isOnEC _ O = True
isOnEC curve (Point x y) = (y * y) == evalPoint curve x

-- runs a trusted setup ceremony: [g * s^0, g * s^1, g * s^2, ...]
-- `secret` can be discarded once the list is shared with the prover and the verifier
getPowerCycle g s = g : takeWhile (/= g) (tail $ iterate (`mul` s) g)

neg O = O
neg (Point x y) = Point x (-y)

double O = O
double (Point x y)
  | y == 0    = O
  | otherwise = Point x' y'
  where
  lambda = ((x <^> (2 :: Int)) * 3) / (y * 2)
  x'     = (lambda <^> (2 :: Int)) - (x * 2)
  y'     = (lambda * (x - x')) - y

add O b = b
add a O = a
add a@(Point x_A y_A) (Point x_B y_B)
  | x_A == x_B = if y_A == (-y_B) then O else double a
  | otherwise  = Point x' y'
  where
  lambda = (y_B - y_A) / (x_B - x_A)
  x'     = (lambda <^> (2 :: Int)) - x_A - x_B
  y'     = (lambda * (x_A - x')) - y_A

sub a b = add a (neg b)

-- multiply point by scalar. IMPORTANT: the scalar must belong to F_p where p
-- is the curve order, NOT the underlying field of point coordinates!
mul point n
  | intN < 0  = error "negative scalar n"
  | otherwise = mult' point $ reverse binary
  where
  intN = toInteger n
  -- converts a number to its binary representation (as a list)
  -- it speeds up calculations by doubling the point along the scalar's binary representation
  binary = Data.Foldable.toList $ unfoldl (\d -> if d == 0 then Nothing else Just $ d `divMod` 2) intN
  mult' _ [] = O
  mult' p (n:ns) = add (if n == 0 then O else p) $ mult' (double p) ns

-- function represents the line between P1 and P2, and evaluates at T
lineFunc (Point x1 y1) (Point x2 y2) (Point xt yt)
  | x1 /= x2  = m1 * (xt - x1) - (yt - y1)
  | y1 == y2  = m2 * (xt - x1) - (yt - y1)
  | otherwise = xt - x1
  where
  m1 = (y2 - y1) / (x2 - x1)
  m2 = 3 * (x1 <^> (2 :: Int)) / (2 * y1)

-- generic pairing function. It takes `ateLoopCount` as a loop counter argument
pairingGeneric fieldModulus curveOrder g2 ateLoopCount b b2 twist pFun q p
  | q == O || p == O                    = oneFq
  | isOnEC (0, b2) q && isOnEC (0, b) p = millerPairing fieldModulus curveOrder ateLoopCount pFun (twist q) pInGt
  | otherwise                           = error "incorrect arguments"
  where
  oneFq = toOne $ x $ twist g2
  pInGt = Point (fromIntegral (x p) * oneFq) (fromIntegral (y p) * oneFq)

-- generic main Miller loop. It takes `pFun` as an argument to post process the
-- result of Miller loop execution
millerPairing fieldModulus curveOrder ateLoopCount pFun q p = f <^> ((fieldModulus^12 - 1) `div` curveOrder)
    where
    -- the function `toOne` returns 1 in the given field in a generic way
    f          = pFun q p $ millerLoop iterations (toOne (x p), q)
    -- list of true/false per bits of operand
    iterations = tail $ reverse $
        unfoldr (\b -> if b == (0 :: Integer) then Nothing
                      else Just(odd b, shiftR b 1)) ateLoopCount
    -- main loop
    millerLoop [] res = res
    millerLoop (i:iters) (f, r) = millerLoop iters $ if i then (f2, r2) else (f1, r1) 
      where
      f1 = f * f * lineFunc r r p
      r1 = double r
      f2 = f1 * lineFunc r1 q p
      r2 = r1 `add` q
