module Main where

import Test.Framework
import Data.Binary.Tests.Vector
import Data.Binary.Tests.Repa

main = defaultMain
    [Data.Binary.Tests.Vector.tests
    ,Data.Binary.Tests.Repa.tests
    ]
