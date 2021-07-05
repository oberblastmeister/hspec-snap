module PathSpec (spec) where

import Data.Maybe (fromJust)
import Path
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "path validation" $ do
    it "should find extension" $
      fromJust (fileExtension [relfile|test.snap|]) `shouldBe` ".snap"

    it "should be able to add extension from path that alreaady has one" $
      toFilePath
        (fromJust (addExtension ".new" [relfile|test.snap|]))
        `shouldBe` "test.snap.new"

    it "should be able to add extensions multiple times" $ do
      let add ext p = fromJust $ addExtension ext p
      toFilePath (add ".new" (add ".snap" [relfile|test|])) `shouldBe` "test.snap.new"