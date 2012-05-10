module Main where

import Test.Framework
import Data.Binary.Tests.Vector

main = defaultMain
    [Data.Binary.Tests.Vector.tests
    ]
