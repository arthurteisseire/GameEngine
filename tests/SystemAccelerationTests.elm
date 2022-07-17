module SystemAccelerationTests exposing (..)

import ComponentKeyboardInput
import EntityTable exposing (..)
import Expect exposing (..)
import KeyboardInput
import SystemAcceleration
import Test exposing (..)


suite : Test
suite =
    describe "System Acceleration Tests"
        [ test
            "Simple update"
            (\_ ->
                let
                    entity =
                        EntityId 1

                    actualTables =
                        { a =
                            emptyComponentTable
                                |> setComponent entity { key = Just KeyboardInput.Right }
                        , b =
                            emptyComponentTable
                                |> setComponent entity { x = 0, y = 0 }
                        }

                    expectedTables =
                        { a =
                            emptyComponentTable
                                |> setComponent entity { key = Just KeyboardInput.Right }
                        , b =
                            emptyComponentTable
                                |> setComponent entity { x = 1, y = 0 }
                        }
                in
                Expect.equal
                    (SystemAcceleration.update actualTables)
                    expectedTables
            )
        ]