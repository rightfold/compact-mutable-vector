module Data.Vector.Mutable.CompactSpec
  ( spec
  ) where

import Data.Foldable (for_)
import Test.Hspec (Spec, it, shouldBe)
import System.Mem (performGC)

import qualified Data.Compact as Compact
import qualified Data.Vector.Mutable.Compact as CVector

spec :: Spec
spec =
  for_ [0 .. 63] $ \n ->
    it (show n) $ do
      c <- Compact.compact ()
      v <- CVector.replicate c n "ABC"
      for_ [0 .. n - 1] $ \i -> do

        performGC

        abc <- CVector.read v i
        abc `shouldBe` "ABC"

        performGC

        CVector.write v i (show i)
        def <- CVector.read v i
        def `shouldBe` (show i)
