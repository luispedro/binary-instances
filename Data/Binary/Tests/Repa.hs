{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Data.Binary.Tests.Repa
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import qualified Data.Array.Repa.Index as R

import Data.Binary
import Data.Binary.Array.Repa
import Data.Array.Repa.Repr.Unboxed

tests = $(testGroupGenerator)

type IArray = R.Array R.U R.DIM1 Int

instance Arbitrary IArray where
    arbitrary = do
        elems <- arbitrary
        return (R.fromList (R.ix1 $ length elems) elems)

prop_ed_id v = (decode . encode) v `eqTo` v
    where
        _types = v :: IArray

eqTo :: IArray -> IArray -> Bool
eqTo v0 v1 = 0 == (R.sumAllS (v0 R.-^ v1))

