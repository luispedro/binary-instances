{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Data.Binary.Generic () where

import Data.Binary
import GHC.Generics

instance Binary (U1 p) where
  put U1 = return ()
  get = return U1

instance (Binary (f p), Binary (g p)) => Binary ((:*:) f g p) where
  put (a :*: b) = put a >> put b
  get = do
    a <- get
    b <- get
    return (a :*: b)

instance (Binary (f p), Binary (g p)) => Binary ((:+:) f g p) where
  put (L1 x) = put False >> put x
  put (R1 x) = put True >> put x
  get = do
    v <- get :: Get Bool
    if v
        then get >>= (return . L1)
        else get >>= (return . R1)

instance (Binary (f p)) => Binary (M1 i c f p) where
  put (M1 x) = put x
  get = get >>= (return . M1)

instance (Binary c) => Binary (K1 i c p) where
  put (K1 x) = put x
  get = get >>= (return . K1)
