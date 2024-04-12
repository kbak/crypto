{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll
module FRITest where

import ModularArithmetic
import Blake
import MerkleTree
import FFT
import FRI
import Test.QuickCheck

fieldModulus = 2^256 - 2^32 * 351 + 1

-- ensures the right modulus is loaded
prop_correctModulus = (2^256 - 2^32 * 351 + 1) `mod` fieldModulus == 0

-- pure FRI tests
nPolyCoeffs = 4096 :: Integer
poly = [0 .. nPolyCoeffs - 1]
rootOfUnity = pow 7 ((fieldModulus - 1) `div` 16384) fieldModulus
evs = fftMain poly fieldModulus rootOfUnity False
evaluations = map Right evs

proof = proveLowDegree evaluations rootOfUnity nPolyCoeffs fieldModulus 0

-- the full proof is about 100kb long
testFriLength = friProofBinLength proof

directProof = [ "c1fbbd6777b922ecc83a9bc2ba541e158eca8ceead4961ae899ea82fa02be"
              , "111fc4b0b5bed9a74034865bda7ae562554c50c5bd82a0abd1859b25944eda"
              , "8983faf143e17c167d93dd6796faa49d5681aa3f473764113eb79a7d1c40b3"
              , "e77b6820813ccaf45b403fd17761caedc10f229f387d45291adbe76d1545062"
              , "924e3ea43f8612ae521b99372ae8605fe4f9dcdf28333830c769b5a2899df55"
              , "bba8a4be7b33f0a574c145c075ea33a5359e999878d493dc6e60aeca0dc624c"
              , "dd2564d133c888f139f68e647cf1bdcf9bc17a717f43bb8dfa937a3a9af67a"
              , "d03fbe5e96a15d5be8e4f1805b31a279fce1815a2835e06f3bbd854fa495d195"
              , "af9b31a779b0d01293e794f76edd65ce9d79f6692d4d7ae5e6278b74a72d"
              , "432e35bce87e2375e57984858db1804e310ba897f734ab24d5828643d8b"
              , "ab9c62acbc54d969c5a451a3b36efbfa388e4fea9de773f4981c1e70a5e5ea42"
              , "d465a5fe2363ec4a844b6047924beecbe5507ed0fe612222133dfb6745abb851"
              , "f3fc33f32348d3cdf39761fa47a7512c21c3a1165772b85eef6fe1ac8d9b8df"
              , "ed569ee5448560aa8c885997e535256998f31a7f75faa29f7d6c890f0fc8e"
              , "7467f668b89a74852a1da8623cfccd39dc01ad7705221b5d4ecd73adeaa9e1"
              , "742b771f57fa40b65a35ffaa6a0236b6d693a3030e24e38f3a5df73cebb998e"
              , "8d96ae74f8aca23183c52dcfdfda6c274c094dfced3c5f896d31baaf1cf730"
              , "280aa7aa790ac79fefb05d1ba07a7edf49888c87b85451808a17d31bb17e24"
              , "b6d3835a461b0df633bafedd7f907a294fcd1aabff39b9fc4c6a272d687c4"
              , "b1a8faec6fa066d21ebf15b291dfe4c5d46c255763524f3d29545877fd2cffdb"
              , "c0c6912b88bbffbbd1de257ecb5e4bbb1dfad7bba66f4c2f883b57d8dab07b"
              , "6f72f4d3b103e11ca78b5eb1964b6024c128b51e615eec10221d70aee01d26"
              , "415458e2769ba7e9ce0a6aae8a376d52f97372a67499724e8ed19429f6265"
              , "f0e4f816c2955673424a97a3877e70182a8f6fb436ffe36321fe6879b2119b"
              , "9ec7cea11f11e2eba6ddbcda89976bf5107b4fb5533dcb37f99a67344f97"
              , "a2351ab3cd996371e8cf154392624fef7f1126db15dd9153465a3bea12a"
              , "ce4ad5d8edee5aee0ba45e18b1123bf73940cb11c9cac1b960ec5ba38b3e6a"
              , "678894e55257b347898ce6b57acd68738366d9d1e63f5bb8937cd285e9f73a"
              , "3d71d4a1d5bde8f0c99110fde3faf2a21a7a793363bf5a69a60eb4829fd57c5"
              , "dbd6d8f33d3a8a886876377b3b5cacb7a0bc83fcc35dc362c095f8609aa3e8db"
              , "277acf9c1544b344e59e35aa7d8e6b3cb2eb08b7dd8c513841995318473876e"
              , "3528c1705a5dfe80de2a1d6bcac6d02b3ced248eea7b8040ea4e214f7e94d8"
              , "852cfca644ccbb2264f934bb611a84caecf45f507031639297d9fd4b8734cb65"
              , "7758a18ca48225fba63af89dfae3e82edfebd2aaa32ab4e7544baa4135d2578"
              , "eda93b2ae874658b09b1bb9bc7dd74f78d1ed487f3b113387853f3bc2504c2"
              , "0e9e0bd876607b61e629ed89b55b0b6ee9ee635735eca33f8153b48ed2329"
              , "cce824aab3db8d540615966e3f55a6bb84117daa26d17e9553f4ee46c3f5"
              , "731a9ed257a519d62ceb14dd90cc6fc1a32c639219a7621b8e94eae9283f166"
              , "d38c7c558e5cf55e1bb6b6946acaa244b4fe8642f8c0aead2347d93810d6f2"
              , "42e12c9fed1f79931e9c2eb21d866894903c6372a6e9fc281b11317bb93f30"
              , "6b922b4277a87461de4b8c6af48c701f1ae3ae2590ad73e7647c0fca7354733"
              , "3960c0544a9cb940db5c87f7b80e7b1da92181dcdb4e93dcacf3e78165"
              , "4259037a03eb23e477159879119ed517eea60144dd302c12ad6c837ab832fe"
              , "e335e79eb17fa2363046491855f7b2f1a3b673e5322fcdd5b1fe213cd1a41e4"
              , "499368210dd7688f81443f7f6d6bddabbede6e539f37af5a778f51a58ee64f"
              , "c858263cf7502aed544e0d6e05f9da4c6e297537acff67fc1c63ed2e3714f2"
              , "ef7952ba526fd87ce2e7bb37d74d22be9f438ee05038ec4374e0a8afc44f63bb"
              , "1174e4eccf9645d1ae551e980b12b1367e77433727023d8e85d44e4319aa74"
              , "7cc34ad790ecd413462d6699c43762d365282a92e3758ac5a646c8dc5f4c9f9a"
              , "e66431beb3108eca7610e37975a13f11da247f431276befc79f52043fa314ce5"
              , "e2a1f345c7172d891b78ac087bcb1f3df59a77f7c60ba40e8a75fea26dec0"
              , "c650d52d78c94912f7714db373c9d1b2e7c2dfe8ce3ebde57a4e14e5b4b8b6"
              , "165dc5596694e97eab8f3571dba948317cc848f7aeb9b9f40bcfc7c458fddeb"
              , "cda6718f25696b39e87110c66b10c483e254841985af3dcacbad937f3fdcf"
              , "bf0dfe71230b6120faad5a4c87f631b0f9fe68e3f3f3cc1b74b5fb92119"
              , "f6293a90574e7e278b824eb121f5354552bb59ad384831edcce398887535d4"
              , "5abec7a5fe8a643a0b3f027bac62c30b4b44aa7735c24d8155981e5764ef9d"
              , "94b5d7f10ab2641198e8d32e32e5fa49d618954e94c68af96e64b5cd46e"
              , "47228aac06a13669b7d1b48d51634526da11229ffd9389d1bbba397d526436"
              , "bc4a149aa04bbb24abb9bea51421cf8596fcd730852cb7cb4729be3ada8"
              , "78bbaa84a13639677d6d03f974ca935f4eee1c1b82f8ef4684a44f484cc8775"
              , "4851a7e8d13d762185f36b66668c4bc619ff640d616d425e3d6a18d9a5b7c24"
              , "f384c24d8cd2a91424efff1118a14d5b852652e35615f682e6893cf4257bbdd"
              , "50351c0f2c3a27e549834dd6211cb4e7ae81a119bbc6f2a0dff9f636f168" ]

prop_proveLowDegreeDirect = directProof == showMerkle (direct proof)

prop_fri = verifyLowDegreeProof (merkelize evaluations!!1) rootOfUnity proof (fromInteger nPolyCoeffs) fieldModulus 0

fakedata = [Right $ if pow 3 i nPolyCoeffs > 400 then x else 39 | (x, i) <- zip [0..] evs]
proof2 = proveLowDegree fakedata rootOfUnity nPolyCoeffs fieldModulus 0

prop_wrongFri1 = not $ verifyLowDegreeProof (merkelize fakedata!!1)
                       rootOfUnity proof (fromInteger nPolyCoeffs) fieldModulus 0

prop_wrongFri2 = not $ verifyLowDegreeProof (merkelize evaluations!!1)
                       rootOfUnity proof (fromInteger nPolyCoeffs `div` 2) fieldModulus 0

return []
runTests = $quickCheckAll