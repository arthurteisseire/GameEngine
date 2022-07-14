module SystemCollisionTests exposing (..)

import EntityTable exposing (..)
import Expect exposing (..)
import SystemCollision
import Test exposing (..)


suite : Test
suite =
    describe "System collision tests"
        [ test
            "Update empty table"
            (\_ ->
                Expect.equal
                    (SystemCollision.update { a = emptyComponentTable, b = emptyComponentTable })
                    { a = emptyComponentTable, b = emptyComponentTable }
            )
        , test
            "Simple update"
            (\_ ->
                let
                    entity =
                        EntityId 1

                    actualTables =
                        { a =
                            emptyComponentTable
                                |> setComponent entity { x = 0, y = 0 }
                        , b =
                            emptyComponentTable
                                |> setComponent entity { x = 1, y = 0 }
                        }

                    expectedTables =
                        { a =
                            emptyComponentTable
                                |> setComponent entity { x = 1, y = 0 }
                        , b =
                            emptyComponentTable
                                |> setComponent entity { x = 0, y = 0 }
                        }
                in
                Expect.equal
                    (SystemCollision.update actualTables)
                    expectedTables
            )
        ]
