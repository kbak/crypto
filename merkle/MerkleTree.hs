module MerkleTree where
-- implements stuff from https://github.com/ethereum/research/blob/master/mimc_stark/merkle_tree.py

import Basic
import Data.Array
import qualified Data.Map
import Data.Bits
import Data.Word
import Data.List
import Data.Maybe
import qualified Data.ByteString as B
import Blake

-- shows Merkle Tree in a more readable form
showMerkle = map toHex

-- returns a list of ByteStrings that represents a Merkle tree with root as the first element
merkelize l = B.empty : concat (reverse $ takeWhile (/= []) $ iterate merkelize' $ map toBytes l)
    where
    merkelize' (l1:l2:ls) = blake (B.append l1 l2) : merkelize' ls
    merkelize' _ = []

-- returns Merkle Tree as an array
merkelizeA l = listArray (0, 2 * fromIntegral (length l) - 1) (merkelize l)

-- given an index, returns all elements of the Merkle tree that depend on that index
mkBranch tree index = map (tree!) indices
    where
    index'  = index + fromIntegral (length tree) `div` 2
    indices = index' : unfoldr (\i -> if i <= 1 then Nothing else Just (i `xor` 1, i `div` 2)) index'

-- verifies if proof is valid, i.e., it matches the Merkle's tree root
verifyBranch root index proof@(p0:ps) outputAsInt
    | root == root' = if outputAsInt then Right $ bytesToInt p0 else Left p0
    | otherwise     = Left B.empty -- error "Incorrect proof"
    where
    index' = index + (2 ^ length proof)
    root'  = fst $ Prelude.foldl computeHash (p0, index') ps
    computeHash (v, i) p = (blake $ uncurry B.append (if odd i then (p, v) else (v, p)), i `div` 2)

-- make a compressed proof for multiple indices
mkMultiBranch tree indices = snd $ Data.List.mapAccumL elideBranch Data.Map.empty newBranches
    where
    halfLen = fromIntegral (length tree) `div` 2
    -- elements in the tree we can get from the branches themselves
    newBranches       = map (\i -> (halfLen + i, mkBranch tree i)) indices
    initialIndices    = concatMap calculateIndices newBranches
    calculableIndices = calcParents $ Data.Map.fromList $ map (, True) initialIndices

    calculateIndices (index, newBranch) = take (length newBranch) indices
        where
        indices = map snd $ iterate (\(i, _) -> (i `div` 2, i `xor` 1)) (index, index)

    -- fill in the calculable list: if we can get or calculate both children, we can calculate the parent
    calcParents indicesMap
        | null toAdd = indicesMap
        | otherwise  = calcParents (Data.Map.union indicesMap (Data.Map.fromList $ map (, True) toAdd))
        where
        ks          = reverse [k `div` 2 | k <- Data.Map.keys indicesMap]
        isAdded k   = Data.Map.member k indicesMap
        isMissing k = isAdded (2 * k) && isAdded (1 + 2 * k) && not (isAdded k)
        toAdd       = filter isMissing ks

    -- if for any branch node both children are calculable, or the node overlaps with a leaf, or the node
    -- overlaps with a previously calculated one, elide it
    elideBranch scanned (index, n:ns) = (scanned', n : branch')
        where
        ((_, scanned'), branch') = Data.List.mapAccumL reviewNode (index, Data.Map.insert index True scanned) ns

    reviewNode (index, scanned) node = ((index `div` 2, scanned'), node')
        where
        index1      = index `xor` 1
        scanned'    = Data.Map.insert index1 True scanned
        isAdded i   = Data.Map.member i calculableIndices
        node'       = if isRedundant then B.empty else node
        isRedundant = isAdded (index1 * 2) && isAdded (index1 * 2 + 1) ||
                      (index1 - halfLen) `elem` indices || Data.Map.member index1 scanned

-- there is clash of (!) imported both from Array and Map
-- convenience function that behves like (!) for Maps
getValue k m = fromJust $ Data.Map.lookup k m

-- verify a compressed proof
verifyMultiBranch root indices proof = [verifyBranch root i b False | (i, b) <- zip indices proof']
    where
    -- fill in elements from the branches
    partialTree = calcParents $ foldl addBranch Data.Map.empty (zip indices proof)
    proof'      = snd $ Data.List.mapAccumL reviewProof partialTree (zip indices proof)

    addBranch pTree (i, branch@(n:ns)) = snd $ foldl addRemaining (index, Data.Map.insert index n pTree) ns
        where
        halfTreeSize = 2 ^ (length branch - 1)
        index = halfTreeSize + i

    addRemaining (index, pTree) node = (index `div` 2, pTree')
        where
        pTree' = if B.empty == node then pTree else Data.Map.insert (index `xor` 1) node pTree

    -- if we can calculate or get both children, we can calculate the parent
    calcParents pTree
        | null toAdd = pTree
        | otherwise  = calcParents (Data.Map.union pTree (Data.Map.fromList digests))
        where
        ks          = reverse [k `div` 2 | k <- Data.Map.keys pTree]
        isAdded k   = Data.Map.member k pTree
        isMissing k = isAdded (2 * k) && isAdded (1 + 2 * k) && not (isAdded k)
        toAdd       = filter isMissing ks
        digests     = map (\k -> (k, blake $ B.append (getValue (2 * k) pTree) (getValue (2 * k + 1) pTree))) toAdd

    -- if any branch node is missing, we can calculate it
    reviewProof pTree (i, proof@(p:ps)) = (pTree', p : ps')
        where
        halfTreeSize = 2 ^ (length proof - 1)
        index = halfTreeSize + i
        ((_, pTree'), ps') = Data.List.mapAccumL reviewNode (index, pTree) ps

    reviewNode (index, pTree) node
        | B.empty == node' = error "Empty node"
        | otherwise        = ((index `div` 2, pTree'), node')
        where
        index1 = index `xor` 1
        node'  = if node == B.empty then getValue index1 pTree else node
        pTree' = Data.Map.insert index1 node' pTree

-- byte length of a multi proof
binLength proof = (sum [B.length (B.concat x) + (length x `div` 8) | x <- proof]) + length proof * 2
