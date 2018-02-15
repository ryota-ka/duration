{-# LANGUAGE QuasiQuotes #-}

module Data.Time.Clock.MsSpec (spec) where

import Data.Time.Clock.Ms (ms)

import Test.Hspec (context, describe, it, shouldBe, Spec)

spec :: Spec
spec = do
    describe "ms" $ do
        context "when given a short string" $ do
            it "should preserve ms" $ do
                [ms| 100 |] `shouldBe` 0.1

            it "should convert from m to DiffTime" $ do
                [ms| 1m |] `shouldBe` 60

            it "should convert from h to DiffTime" $ do
                [ms| 1h |] `shouldBe` 3600

            it "should convert d to DiffTime" $ do
                [ms| 2d |] `shouldBe` 172800

            it "should convert w to DiffTime" $ do
                [ms| 3w |] `shouldBe` 1814400

            it "should convert s to DiffTime" $ do
                [ms| 1s |] `shouldBe` 1

            it "should convert ms to DiffTime" $ do
                [ms| 100ms |] `shouldBe` 0.1

            it "should work with decimals" $ do
                [ms| 1.5h |] `shouldBe` 5400

            it "should work with multiple spaces" $ do
                [ms| 1   s |] `shouldBe` 1

            it "shoule be case-insensitive" $ do
                [ms| 1.5H |] `shouldBe` 5400

            it "should work with numbers starting with ." $ do
                [ms| .5ms |] `shouldBe` 0.0005

            it "should work with negative integers" $ do
                [ms| -100ms |] `shouldBe` -0.1

            it "should work with negative decimals" $ do
                [ms| -1.5h |] `shouldBe` -5400

            it "should work with negative decimals starting with '.'" $ do
                [ms| -.5h |] `shouldBe` -1800

        context "when given a long string" $ do
            it "should convert milliseconds to DiffTime" $ do
                [ms| 53 milliseconds |] `shouldBe` 0.053

            it "should convert msecs to DiffTime" $ do
                [ms| 17 msecs |] `shouldBe` 0.017

            it "should convert sec to DiffTime" $ do
                [ms| 1 sec |] `shouldBe` 1

            it "should convert from min to DiffTime" $ do
                [ms| 1 min |] `shouldBe` 60

            it "should convert from hr to DiffTime" $ do
                [ms| 1 hr |] `shouldBe` 3600

            it "should convert days to DiffTime" $ do
                [ms| 2 days |] `shouldBe` 172800

            it "should work with decimals" $ do
                [ms| 1.5 hours |] `shouldBe` 5400

            it "should work with negative integers" $ do
                [ms| -100 milliseconds |] `shouldBe` -0.1

            it "should work with negative decimals" $ do
                [ms| -1.5 hours |] `shouldBe` -5400

            it "should work with negative decimals starting with '.'" $ do
                [ms| -.5 hr |] `shouldBe` -1800
