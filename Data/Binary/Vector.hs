module Data.Binary.Vector () where

import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import Data.Binary

instance (VP.Prim a, Binary a) => Binary (VP.Vector a) where
    put = vput
    get = vget

instance (VS.Storable a, Binary a) => Binary (VS.Vector a) where
    put = vput
    get = vget

instance (VU.Unbox a, Binary a) => Binary (VU.Vector a) where
    put = vput
    get = vget

instance (Binary a) => Binary (V.Vector a) where
    put = vput
    get = vget

vput :: (Binary a, GV.Vector v a) => v a -> Put
vput v = put (GV.length v) >> put `GV.mapM_` v

vget :: (Binary a, GV.Vector v a) => Get (v a)
vget = (get :: Get Int) >>= (`GV.replicateM` get)
