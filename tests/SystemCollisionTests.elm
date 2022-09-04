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
                    (SystemCollision.updateWorld emptyEntitySet emptyTable emptyTable emptyTable)
                    { tableA = emptyTable, tableB = emptyTable }
            )
        , test
            "Simple update"
            (\_ ->
                let
                    ( entityTable, entityId ) =
                        emptyEntitySet |> addEntity

                    actualPositionTable =
                        emptyTable |> insertComponent entityId { x = 0, y = 0 }

                    actualVelocityTable =
                        emptyTable |> insertComponent entityId { x = 1, y = 0 }

                    expectedTables =
                        { tableA = emptyTable |> insertComponent entityId { x = 1, y = 0 }
                        , tableB = emptyTable |> insertComponent entityId { x = 0, y = 0 }
                        }
                in
                Expect.equal
                    (SystemCollision.updateWorld
                        entityTable
                        actualPositionTable
                        actualPositionTable
                        actualVelocityTable
                    )
                    expectedTables
            )
        ]
