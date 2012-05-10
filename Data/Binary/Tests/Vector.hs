{-# LANGUAGE TemplateHaskell #-}
module Data.Binary.Tests.Vector
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import qualified Data.Vector as V

import Data.Binary
import Data.Binary.Vector

tests = $(testGroupGenerator)

instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

prop_ed_id v = (decode . encode) v == v
    where
        _types = v :: (V.Vector Int)

