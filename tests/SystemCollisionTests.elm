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
                    (SystemCollision.update { a = emptyTable, b = emptyTable })
                    { a = emptyTable, b = emptyTable }
            )
        , test
            "Simple update"
            (\_ ->
                let
                    entity =
                        EntityId 1

                    actualTables =
                        { a =
                            emptyTable
                                |> setComponent entity { x = 0, y = 0 }
                        , b =
                            emptyTable
                                |> setComponent entity { x = 1, y = 0 }
                        }

                    expectedTables =
                        { a =
                            emptyTable
                                |> setComponent entity { x = 1, y = 0 }
                        , b =
                            emptyTable
                                |> setComponent entity { x = 0, y = 0 }
                        }
                in
                Expect.equal
                    (SystemCollision.update actualTables)
                    expectedTables
            )
        ]
