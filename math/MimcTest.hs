module MIMCTest where

import MIMC
import Data.Array

-- number of roundConstants rounds
nRounds = 64
roundConsts = array (0, nRounds - 1) ((0 :: Int, 1) : [(i, fromIntegral i * roundConsts!(i - 1)) | i <- [1 .. nRounds - 1]])
