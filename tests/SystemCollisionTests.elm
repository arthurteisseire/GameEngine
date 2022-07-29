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
                    (SystemCollision.update emptyEntityTable emptyTable emptyTable emptyTable)
                    { tableA = emptyTable, tableB = emptyTable }
            )
        , test
            "Simple update"
            (\_ ->
                let
                    ( entityTable, entityId ) =
                        emptyEntityTable |> addEntity

                    actualPositionTable =
                        emptyTable |> setComponent entityId { x = 0, y = 0 }

                    actualVelocityTable =
                        emptyTable |> setComponent entityId { x = 1, y = 0 }

                    expectedTables =
                        { tableA = emptyTable |> setComponent entityId { x = 1, y = 0 }
                        , tableB = emptyTable |> setComponent entityId { x = 0, y = 0 }
                        }
                in
                Expect.equal
                    (SystemCollision.update
                        entityTable
                        actualPositionTable
                        actualPositionTable
                        actualVelocityTable
                    )
                    expectedTables
            )
        ]
