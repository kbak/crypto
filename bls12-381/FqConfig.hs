module FqConfig where

-- bls12 381
-- see some details in https://ask.sagemath.org/question/49663/efficiently-computing-tower-fields-for-pairings/
fieldModulus = 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787
-- polynomial: w^2 + 1
fq2Modulus = [1, 0, 1]
-- polynomial: w^12 - 2 * w^6 + 2, if x = w^6 then we have w^2 - 2x + 2
fq12Modulus = [2, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0, 1]

-- If u signifies the other dimension, for a number a + b*u multiplying by
-- u + 1 (comes from Fp6 construction) gets us au + a + bu^2 + bu and
-- because u^2 = -1, we get (a - b) + (a + b)u
mulNonResFq2 a0 a1 = (a0 - a1, a0 + a1)

{-
    Tower of extensions
        https://www.ietf.org/archive/id/draft-irtf-cfrg-pairing-friendly-curves-08.html
        https://ask.sagemath.org/question/49663/efficiently-computing-tower-fields-for-pairings/

    GF(p^2) = GF(p)[u] / (u^2 + 1)
    GF(p^6) = GF(p^2)[v] / (v^3 - (u + 1))
    GF(p^12) = GF(p^6)[w] / (w^2 - v)

    In sage:
    F = GF(0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab)
    p = F.characteristic()
    F12.<G> = GF(p^12)    # G is the generator, we will not need it below

    u = sqrt(F12(-1))
    R12.<Y> = PolynomialRing(F12)

    v = (Y^3 - (u+1)).roots(multiplicities=False)[0]
    w = sqrt(v)

    P = w.minpoly()
    
    K.<W> = GF(p^12, modulus=P)

    sage: P                                                                                                                              
    x^12 + 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559785*x^6 + 2

    Q = v.minipoly()

    sage: Q (For Fq6)
    x^6 + 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559785*x^3 + 2

    so that's our `fq12Modulus`
-}