module MainSpec where

import Test.Hspec
import Test.QuickCheck

{-# HLINT ignore "Use camelCase" #-}

{-
spec :: Spec
spec = do
  describe "main" $ do
    it "testing Main" $ do
     import Test.Hspec
-}

main :: IO ()
main = hspec $ describe "read" $ do
    context "when used with ints" $ do
      it "is inverse to show" $ property $
        \x -> (read . show) x == (x :: Int) 


