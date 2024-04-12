module ModularArithmetic where
-- modular arithmetic operations (used in finite fields)
-- some code comes from https://github.com/nccgroup/pairing-bls12381

import Data.Bits

-- makes roots of unity (domain) for a given generator and modulus
getPowerCycle g m = 1 : takeWhile (/= 1) (iterate (\gn -> gn * g `mod` m) g)

invGcdE a m = t `mod` m
  where
  (_, _, t) = gcdE m a

gcdE a 0 = (a, 1, 0)
gcdE a b = (d, t, s - q * t)
  where
  (q, r)    = a `divMod` b
  (d, s, t) = gcdE b r

-- computes the inverse, i.e., a * inv a = 1 mod m
inv = invGcdE

-- multiple inverse (from https://www.vitalik.ca/general/2018/07/21/starks_part_3.html)
multiInv :: [Integer] -> Integer -> [Integer]
multiInv values m = map fst $ reverse outputs
  where
  -- partials contains successive mulitples in reverse  [..., v1v2v3, v1v2, v1]
  partials = reverse $ scanl (\acc v -> (acc * (if v == 0 then 1 else v)) `mod` m) 1 values
  -- inverse of the multiple of all elements
  pInv     = head partials `inv` m
  -- we ignove the highest multiple and zip partials with values
  pvs      = zip (tail partials) (reverse values)
  -- go over partials and values and multiply the inverse in each step
  outputs  = tail $ scanl (\(_, partialInv) (partialValue, v) ->
      ( if v == 0 then 0 else (partialValue * partialInv) `mod` m
      , (partialInv * (if v == 0 then 1 else v)) `mod` m))
      (0, pInv) pvs

-- computes a^x `mod` m
pow :: (Integral a, Integral b, Data.Bits.Bits b) => a -> b -> a -> a
pow a x m = pow' a x 1
  where
  pow' _ 0 res = res `mod` m
  pow' b e res
    | odd e     = pow' b' e' $ (res * b) `mod` m
    | otherwise = pow' b' e' res
    where
    b' = (b * b) `mod` m
    e' = e `shiftR` 1

-- computes legendre symbol
legendre a m = pow a ((m - 1) `div` 2) m

-- tonelli algorithm for finding square roots in a field modulo `modulus`
sqrt :: Integer -> Integer -> Maybe (Integer, Integer)
sqrt n modulus | legendre n modulus /= 1 = Nothing
sqrt n modulus =
  let s = length $ takeWhile even $ iterate (`div` 2) (modulus - 1)
      q = (modulus - 1) `shiftR` fromIntegral s
  in if s == 1
    then let r = pow n ((modulus + 1) `div` 4) modulus
         in Just (r, modulus - r)
    else let z = (2 +) . length
               $ takeWhile (\i -> modulus - 1 /= legendre i modulus)
               $ [2 .. modulus - 1]
         in loop s
            ( pow (fromIntegral z) q modulus)
            ( pow n ((q + 1) `div` 2) modulus )
            ( pow n q modulus )
  where
    modp element = element `mod` modulus
    loop x c r t
      | modp (t - 1) == 0 = Just (r, modulus - r)
      | otherwise = loop i c' r' t'
      where
        i = (1 +) . length . take (x - 2)
              $ takeWhile (\t2 -> modp (t2 - 1) /= 0)
              $ iterate (\t2 -> modp (t2 * t2))
              $ modp (t * t)
        b  = pow c ((2 ^ (x - i - 1)) :: Integer) modulus
        r' = modp (r * b)
        c' = modp (b * b)
        t' = modp (t * c')