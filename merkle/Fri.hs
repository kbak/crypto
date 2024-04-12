module FRI where

import Basic
import Blake
import ModularArithmetic
import Polynomial
import ModularPolynomial
import Gf
import MerkleTree
import Data.List
import Data.Array
import Data.Either
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

data PartialProof = PartialProof { root :: ByteString
                                 , columnBranches :: [[ByteString]]
                                 , polyBranches   :: [[ByteString]]
                                 } deriving (Eq, Show)

data FRIProof = FRIProof { parts  :: [PartialProof]
                         , direct :: [ByteString]
                         } deriving (Eq, Show)

-- a FRI proof is composed of list of triples (root (32 bytes), multi path, multi path) and values at the end
friProofBinLength (FRIProof ps dir) = sum [32 + binLength br1 + binLength br2 | PartialProof _ br1 br2 <- ps] + (B.length.B.concat) dir

-- generate an FRI proof that the polynomial that has the specified
-- values at successive powers of the specified root of unity has a
-- degree lower than maxdegPlus1
--
-- We use maxdeg+1 instead of maxdeg because it's more mathematically
-- convenient in this case.

proveLowDegree values rootOfUnity maxdegPlus1 m excludeMultiplesOf
    -- if the degree we are checking for is less than or equal to 32,
    -- use the polynomial directly as a proof
    | maxdegPlus1 <= 16          = FRIProof [] $ map toBytes values
    | length values /= length xs = error "Incorrect arguments"
    | otherwise                  = proof{ parts = newPart : parts proof }
    where
    -- calculate the set of x coordinates
    xs = getPowerCycle rootOfUnity m
    -- put the values into a Merkle tree. This is the root that the
    -- proof will be checked against
    mTree = merkelizeA values
    -- select a pseudo-random x coordinate
    specialX = bytesToInt (mTree!1) `mod` m
    -- calculate the "column" at that x coordinate
    -- (see https://vitalik.ca/general/2017/11/22/starks_part_2.html)
    -- We calculate the column by Lagrange-interpolating each row, and not
    -- directly from the polynomial, as this is more efficient
    quarterLen = length xs `div` 4
    vs     = map (either bytesToInt id) values
    vs'    = Data.List.transpose $ chunks quarterLen $ take (4 * quarterLen) vs
    xs'    = Data.List.transpose $ chunks quarterLen $ take (4 * quarterLen) xs
    xPolys = multiLagrange4 xs' vs' m
    column = map (\p -> Right $ polyeval4 p specialX `mod`  m) xPolys
    mTree2 = merkelizeA column
    root2  = mTree2!1
    -- pseudo-randomly select y indices to sample
    ys = getPseudorandomIndices root2 (fromIntegral $ length column) 40 excludeMultiplesOf
    -- compute the positions for the values in the polynomial
    polyPositions = concat [[y + (fromIntegral (length xs) `div` 4) * j | j <- [0..3]] | y <- ys]
    -- this component of the proof, including Merkle branches
    newPart = PartialProof root2 (mkMultiBranch mTree2 ys) (mkMultiBranch mTree polyPositions)
    proof   = proveLowDegree column (pow rootOfUnity (4 :: Integer) m) (maxdegPlus1 `div` 4) m excludeMultiplesOf

-- verify an FRI proof
verifyLowDegreeProof merkleRoot rootOfUnity (FRIProof parts direct) maxdegPlus1 m excludeMultiplesOf
    | not $ and areValidPartial      = False -- error "Partial proofs are invalid"
    | maxdegPlus1' > 16              = False -- error "Incorrect degree"
    | directProofRoot /= merkleRoot' = False -- error "Incorrect merkle root of the direct proof"
    | not $ and areValidDirect       = False -- error "Direct proof is invalid"
    | otherwise                      = True
    where
    -- calculate which root of unity we're working with
    roudeg = fst $ until ((==) 1.snd)
                         (\(roudeg, testval) -> (roudeg * 2, (testval * testval) `mod` m))
                         (1, rootOfUnity)
    -- powers of the given root of unity 1, p, p**2, p**3 such that p**4 = 1
    quarticRootsOfUnity = [1, pow rootOfUnity (roudeg `div` 4) m,
                              pow rootOfUnity (roudeg `div` 2) m,
                              pow rootOfUnity ((roudeg * 3) `div` 4) m]
    -- verify the recursive components of the proof
    ((merkleRoot', rootOfUnity', maxdegPlus1', _), areValidPartial) =
         Data.List.mapAccumL verifyPartialProof (merkleRoot, rootOfUnity, maxdegPlus1, roudeg) parts
    -- verify the direct components of the proof
    directProof = map bytesToInt direct
    (_:directProofRoot:_) = merkelize (map Right directProof)
    -- check the degree of the direct proof data
    intZp a    = intGf a m
    powers     = map intZp $ getPowerCycle rootOfUnity' m
    dirProofIx = [0 .. length directProof - 1]
    pts        = if excludeMultiplesOf /= 0
                 then filter (\i -> i `mod` fromInteger excludeMultiplesOf /= 0) dirProofIx
                 else dirProofIx
    (ls, rs)       = splitAt maxdegPlus1' pts
    poly           = lagrangeGeneric (intZp 1) (selectByIndices powers ls) (selectByIndices (map intZp directProof) ls)
    powersA        = listToArray powers
    directProofA   = listToArray directProof
    areValidDirect = [toInteger (polyeval poly (powersA!i)) == (directProofA!i) | i <- rs]

    verifyPartialProof (mRoot, unityRoot, mDeg, rDeg)
                       (PartialProof root2 cBranches pBranches) =
                       ((root2, pow unityRoot (4 :: Integer) m, mDeg `div` 4, rDeg'), isValid)
        where
        -- calculate the pseudo-random x coordinate
        specialX = bytesToInt mRoot
        rDeg'    = rDeg `div` 4
        -- calculate the pseudo-randomly sampled y indices
        ys = getPseudorandomIndices root2 rDeg' 40 excludeMultiplesOf
        -- compute the positions for the values in the polynomial
        polyPositions = concat [[y + rDeg' * j | j <- [0..3]] | y <- ys]
        -- verify Merkle branches
        columnValues = verifyMultiBranch root2 ys cBranches
        polyValues   = verifyMultiBranch mRoot polyPositions pBranches
        -- for each y coordinate, get the x coordinates on the row, the values on
        -- the row, and the value at that y from the column
        -- the x coordinates from the polynomial
        x1s        = map (\y -> pow unityRoot y m) ys
        xCoords    = map (\y -> map (\r -> (r * y) `mod` m) quarticRootsOfUnity) x1s
        -- the values from the original polynomial
        rows       = chunks 4 $ map bytesToInt $ lefts polyValues
        columnVals = map bytesToInt $ lefts columnValues
        -- verify for each selected y coordinate that the four points from the
        -- polynomial and the one point from the column that are on that y 
        -- coordinate are on the same deg < 4 polynomial
        polys   = multiLagrange4 xCoords rows m
        isValid = and [(polyeval4 p specialX `mod` m) == c | (p, c) <- zip polys columnVals]

selectByIndices xs ids = selectByIndices' 0 xs ids
    where
    selectByIndices' _ [] _ = []
    selectByIndices' _ _ [] = []
    selectByIndices' n (l:ls) (i:ids) = if n == i then l : selectByIndices' (n + 1) ls ids
                                                  else selectByIndices' (n + 1) ls (i:ids)