import Test.Framework (defaultMain)

import qualified Data.Beamable.Tests

main :: IO ()
main = defaultMain
    [ Data.Beamable.Tests.tests
    ]
