import Test.Hspec
import Control.Monad ( mapM_ )
import LevelBattle
--
main :: IO ()
main = hspec $ do

    describe "lb31" $ mapM_
      ( tester "lb31" lb31 )
      [
        ('a' , False),
        ('b' , False),
        ('c' , False),
        ('A' , True),
        ('B' , True),
        ('Z' , True)
      ]

    describe "lb32" $ mapM_
      ( tester2 "lb32" lb32 )
      [
        ( "\NUL super Duper stringifyer", [1,2,3,4,5,6,7,8,9], "\SOH\"vyuky(M"),
        ( "super Duper stringifyer", [1,2,3,4,5,6,7,8,9]  , "twsiw&K}y"),
        ( "Hakell je izvrstan", [1..100]  , "Icniqr'rn*t\134\131\128\130\132r\128")
      ]

    describe "lb33" $ mapM_
      ( testerp "lb33" lb33 )
      [
        ([(+)5, (-)4],  5, True),
        ([(-)2, (-)5], 6, False),
        ([(+)8, (-)89], 7, True),
        ([(+)2, (+)2],  3, False)
      ]

    where
        tester fname f (x, y) =
          it ( fname ++ " " ++ show x ++ " ==> " ++ show y ) $ f x `shouldBe` y
        tester2 fname f (x, y, z) =
          it ( fname ++ " " ++ show x ++ ", " ++ show y ++ " ==> " ++ show z ) $ f x y `shouldBe` z
        testerp fname f (x, y, z) =
          it ( fname ++ " " ++ show y ++ " ==> " ++ show z ) $ f x y `shouldBe` z
        err fname f x =
          it ( fname ++ " " ++ show x ) $ (f x `seq` pure ()) `shouldThrow` anyException
