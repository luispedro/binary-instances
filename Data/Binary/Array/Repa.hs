module Data.Binary.Array.Repa () where

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as R ()
import qualified Data.Array.Repa.Eval as R
import Data.Binary

newtype AsShape s = AsShape s

instance (R.Shape s) => Binary (AsShape s) where
    put (AsShape s) = put $ R.listOfShape s
    get = get >>= (return . AsShape . R.shapeOfList)

instance (Binary e, R.Shape sh, R.Repr r e, R.Fillable r e) => Binary (R.Array r sh e) where
    put arr = (put . AsShape $ R.extent arr) >> (put $ R.toList arr)
    get = do
        AsShape shape <- get
        elements <- get
        return (R.fromList shape elements)

