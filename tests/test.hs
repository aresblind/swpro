{-# OPTIONS_GHC -fno-warn-orphans -}
import SProGame hiding (main)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Applicative
import System.Random

a :: Module
a = Module {modName = "a", importance = 3, reversingTime = 1}
b :: Module
b = Module {modName = "b", importance = 2, reversingTime = 2}
c :: Module
c = Module {modName = "c", importance = 1, reversingTime = 3}

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, props]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
              testCase "No attack" $
                       play [(Target a, Bottom)] @?= NoAttack
            , testCase "No attack 2" $
                       play [(Target a, Bottom), (Target a, Bottom)] @?= NoAttack
            , testCase "Attack prevented on a" $
                       play [(Target b, Target a)
                            ,(Target a, Bottom)] @?= Prevented (Target a)

            , testCase "Attack not prevented on b" $
                       play [(Target b, Target b)
                            ,(Target c, Bottom)
                            ,(Target a, Bottom)] @?= NotPrevented (Target b)
            ]

instance Arbitrary Player where
    arbitrary = elements [Client, Server]

instance Arbitrary Module where
    arbitrary = Module <$>
                suchThat arbitrary (\x -> not (null x))
                <*> suchThat arbitrary (\x -> x >= 0)
                <*> suchThat arbitrary (\x -> x >= 0)

instance Arbitrary Target where
    arbitrary = oneof ([elements [Bottom]] ++ [Target <$> arbitrary])

instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> (arbitrary :: Gen Int)


props :: TestTree
props = testGroup "Quickcheck properties" [
         testProperty "Total value is the sum of importances" $
                          \ss -> totalValue ss === foldl (+) 0 (fmap importance ss)
        , testProperty "Total value is non negative" $
                           \ss -> totalValue ss  >= 0
        , testProperty "Server score ordering" $
                           \ss t  -> let p x y = score Server x NoAttack
                                                 >= score Server x (Prevented y) &&
                                                 score Server x (Prevented y)
                                                 >= score Server x (NotPrevented y)
                                     in
                                       case t of
                                         Target m -> m `elem` ss ==> p ss t
                                         Bottom  -> True ==> p ss t
        , testProperty "Client score ordering" $
                           \ss t  -> let  p x y = score Client x (NotPrevented y)
                                                  >= score Client x (Prevented y) &&
                                                  score Client x NoAttack
                                                  >= score Client x (Prevented y)
                                     in
                                       case t of
                                         Target m -> m `elem` ss ==> p ss t
                                         Bottom  -> True ==> p ss t
        , testProperty "drawFromStrategy gives an index" $
                           \ (NonEmpty seeds) (Positive r) (Positive m) (Positive n) ->  r > 1  ==>
                               let s = replicate r m :: Strategy
                                   ds = \x -> fst (drawFromStrategy (mkStdGen x) s)
                                   out = take n (fmap ds seeds :: [Int])
                               in
                                 minimum out >= 0 && maximum out < length s
        , testProperty "drawFromStrategy respects proportions" $
                           \z (Positive r) (Positive m) ->   r > 1 ==>
                           let
                               seeds = randoms (mkStdGen z) :: [Int]
                               s = replicate r m :: Strategy
                               ds = \x -> fst (drawFromStrategy (mkStdGen x) s)
                               n = 10000
                               out = take n (fmap ds seeds :: [Int])
                               count = \x xs -> length (filter (== x) xs)
                               histogram = [count i out | i <- [0..r-1] ]
                               dh = [ (fromIntegral i) / (fromIntegral (sum histogram)) | i <- histogram]
                               dz = [ (fromIntegral i) / (fromIntegral (sum s)) | i <- s]
                               maxerr = 2 / (sqrt (fromIntegral n :: Double))
                           in
                             whenFail (putStrLn ((show s) ++ " " ++ (show histogram))) $
                                      conjoin [abs (i - j) <= maxerr | (i, j) <- zip dh dz]

        ]
