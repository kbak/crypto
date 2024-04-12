{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module QAP where
-- implementation of R1CS from https://medium.com/@VitalikButerin/quadratic-arithmetic-programs-from-zero-to-hero-f6d558cea649
-- more details: https://www.rareskills.io/post/quadratic-arithmetic-program
-- there exists a Ring Homomorphism from column vectors of dimension n with real number elements to polynomials with real coefficients
-- that's what R1CS -> QAP is about

import Data.Map
import Data.Maybe
import Data.List
import Control.Monad.State.Lazy
import Basic
import Polynomial

-- abstract syntax of flat code
-- TODO: conversion from the nested AST to simple expressions
-- Sub = Add a negative element
-- Div = Multiply by an inverse
data Operator = Add | Mul
    deriving (Eq, Ord, Show, Read)

-- by using a type parameter a, we can work with different types,
-- e.g., Integer, Double, Integer Fp
data Literal a = Variable String | Value a
    deriving (Eq, Ord, Show, Read)

data Rhs a = Simple (Literal a) | Operation (Literal a) Operator (Literal a)
    deriving (Eq, Ord, Show, Read)

data Statement a = Statement String (Rhs a)
    deriving (Eq, Ord, Show, Read)

type FlatCode a = [Statement a]

-- state that we're going to pass in the monad
newtype R1Env a = R1Env { var2asn :: Data.Map.Map String a } deriving (Show)

-- a map from operators types to the actual functions
-- the signature is needed since otherwise the compiler assumes Integer
mapOpFun :: Num a => Map Operator (a -> a -> a)
mapOpFun = fromList [(Add, (+)), (Mul, (*))]

-- interpreter for our simple language
-- produces an assignment to variables (within the R1Env state)
interpret statements m = var2asn $ execState (mapM_ visitStatement statements) (R1Env m)

visitStatement (Statement var rhs) = do
    maybeVal <- visitRhs rhs
    case maybeVal of
        Just v  -> modify (\e -> e { var2asn = Data.Map.insert var v (var2asn e) })
        _       -> return ()

visitRhs rhs = case rhs of
    Simple lit             -> visitLiteral lit
    Operation lit1 op lit2 -> do
        res1 <- visitLiteral lit1
        res2 <- visitLiteral lit2
        visitOperator op res1 res2

visitLiteral lit = case lit of
    Value v    -> return $ Just v
    Variable v -> do
        m <- gets var2asn
        return $ Data.Map.lookup v m

visitOperator op mv1 mv2 = case (mv1, mv2) of
    (Just v1, Just v2) -> return $ Data.Map.lookup op mapOpFun >>= \f -> Just $ f v1 v2
    _                  -> return Nothing

-- runs the code and returns a witness, i.e., an ordered assignment of values to all variables
getWitness vars m flatCode = Prelude.map (\v -> fromJust (Data.Map.lookup v mapVarAsn)) vars
    where
    mapVarAsn = interpret flatCode m

-- s . a * s . b - s . c = 0
isR1cs s (a, b, c) = dotProduct s a * dotProduct s b - dotProduct s c == 0

-- each gate constraint is defined by a triple of vectors (a, b, c)
-- converts a list of gates to a triple of matrices (A, B, C)
gatesToR1cs = Prelude.foldr (\(a, b, c) (as, bs, cs) -> (a:as, b:bs, c:cs)) ([],[],[])

-- produces R1CS (A, B, C) matrices
-- a program can be represented as an R1CS, but evaluating it is not succinct due
-- to the many operations of matrix multiplication.
codeToR1cs vars flatCode = gatesToR1cs $ codeToGates mapVarPos flatCode
    where
    mapVarPos = fromList $ zip vars [1..]

codeToGates m = Prelude.map (statementToGate m)

-- explicit function signature to enforce that the types of a, b, and c are the same
statementToGate :: (Num a) => Map String Int -> Statement a -> ([a], [a], [a])
statementToGate m (Statement var rhs) = (a, b, c)
    where
    (a, b) = rhsToGate m rhs
    c = makeVector m $ maybe [] (\p -> [(p, 1)]) $ Data.Map.lookup var m

rhsToGate m (Simple lit) = (literalToGate m lit, makeVector m [(1, 1)])
rhsToGate m (Operation lit1 op lit2) = operationToGate m op gate1 gate2
    where
    gate1 = literalToGate m lit1
    gate2 = literalToGate m lit2

literalToGate m (Variable v) = makeVector m $ maybe [] (\p -> [(p, 1)]) $ Data.Map.lookup v m
literalToGate m (Value v)    = makeVector m [(1, v)]

operationToGate m Add gate1 gate2 = (zipWith (+) gate1 gate2, makeVector m [(1, 1)])
operationToGate m Mul gate1 gate2 = (gate1, gate2)

-- creates a single gate vector
-- ps is a list of tuples. Each tuple is (variable index, value to set in the vector)
-- all other values are 0
makeVector m ps = [Data.Map.findWithDefault 0 v $ fromList ps | v <- [1.. Data.Map.size m]]

-- transforms an R1CS matrix into a corresponding QAP matrix of polynomials
-- note: we transpose the matrix so that we can later evaluate the polynomials over gate numbers 1,2,3,...
-- to get values for each gate
-- in the end, for each m*n matrix, we end up with m polynomials of n-th degree,
-- where n is the number of gates and m is the number of variables for each gate
r1csMatrixToQap m = transpose $ Prelude.map (lagrange [1..]) (transpose m)

-- QAPs are the primary reason ZK-Snarks are able to be succinct.
-- it is important for the verifier to check the witness succintly (with little resources)
-- the Z polynomial Z poly simply interpolates an identity vector for addition
-- the polynomial has zeros at 1,2,3,4,... so it's the same as [0,0,0,0,...] in R1CS
-- explicit function signature to enforce that the type of z matches the types of a, b, and c
codeToQap :: (Fractional t, Eq t, Enum t) => [String] -> [Statement t] -> ([[t]], [[t]], [[t]], [t])
codeToQap vars flatCode = (r1csMatrixToQap a, r1csMatrixToQap b, r1csMatrixToQap c, z)
    where
    (a, b, c) = codeToR1cs vars flatCode
    -- the polynomial z has as many zero's as there are R1CS gates
    z         = zPoly [1 .. fromIntegral $ length a]

-- evaluate a QAP matrix over points p(1), p(2), ...
-- the result should be "close" to the original R1CS matrix
-- it may not be exact for floating point numbers
qapMatrixToR1cs m = [Prelude.map (`polyeval` x) m' | x <- [1 .. fromIntegral (length $ head m')]]
    where
    m' = transpose m

-- for a QAP matrix M and vector s computes M . s
dotProductQAP m s = Prelude.map (\v -> dotProduct v (Prelude.map fromIntegral s)) m

-- t = A . s * B . s - C . s assuming that A, B, C are QAP matrices and s is a solution vector
-- computes a dot product between the polynomials (QAP matrices) and the witness
-- only prover computes it since it knowns the witness
computePolys (a, b, c) s = (as, bs, cs, t)
    where
    as = dotProductQAP a s
    bs = dotProductQAP b s
    cs = dotProductQAP c s
    t  = (as `polymult` bs) `polysub` cs