module ElmTests exposing (..)

import Expect as Float
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Entity Table tests"
        [ test "Map entities 1"
            (\_ ->
                Float.within (Float.Absolute 0.00000000001) (1 / 2) 0.5
            )
        ]
