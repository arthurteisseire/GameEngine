module EntityTableTests exposing (..)

import Dict
import EntityTable exposing (..)
import Expect exposing (..)
import Fuzz exposing (int, list)
import Test exposing (..)


suite : Test
suite =
    describe "Entity Table tests"
        [ test "Map entities 1"
            (\_ ->
                let
                    ( entityTable, entityId ) =
                        emptyEntityTable |> addEntity

                    numberTable =
                        emptyTable |> setComponent entityId 5

                    updatedNumberTable =
                        mapEntities1 (\_ n -> n + 1) entityTable numberTable
                in
                Expect.equal
                    updatedNumberTable
                    (emptyTable |> setComponent entityId 6)
            )

        --, fuzz2
        --    (list int)
        --    (list int)
        --    "merge then split"
        --    (\xs ys ->
        --        let
        --            tables =
        --                { tableA = Table (Dict.fromList (List.indexedMap (\idx x -> ( idx, x )) xs))
        --                , tableB = Table (Dict.fromList (List.indexedMap (\idx y -> ( idx, y )) ys))
        --                }
        --        in
        --        Expect.equal tables (unionTable2 (splitTable2 (intersectTable2 tables)) tables)
        --    )
        --, fuzz2
        --    (list int)
        --    (list int)
        --    "update 2 tables"
        --    (\xs ys ->
        --        let
        --            tables =
        --                { tableA = Table (Dict.fromList (List.indexedMap (\idx x -> ( idx, x )) xs))
        --                , tableB = Table (Dict.fromList (List.indexedMap (\idx y -> ( idx, y )) ys))
        --                }
        --        in
        --        Expect.equal tables (update2Tables (\tableComp2 -> tableComp2) tables)
        --    )
        ]
