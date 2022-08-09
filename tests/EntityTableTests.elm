module EntityTableTests exposing (..)

import EntityTable exposing (..)
import Expect exposing (..)
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
        , test "Update each entity 2"
            (\_ ->
                let
                    ( entityTable, entityId ) =
                        emptyEntityTable |> addEntity

                    intTable =
                        emptyTable |> setComponent entityId 5

                    floatTable =
                        emptyTable |> setComponent entityId 5.5

                    func _ int float =
                        Tuple2 (int + 1) (float + 2.2)
                in
                Expect.equal
                    (updateEachEntity2 func entityTable intTable floatTable)
                    { tableA = emptyTable |> setComponent entityId 6
                    , tableB = emptyTable |> setComponent entityId 7.7
                    }
            )
        , test "Update each entity 3"
            (\_ ->
                let
                    ( entityTable, entityId ) =
                        emptyEntityTable |> addEntity

                    intTable : Table Int
                    intTable =
                        emptyTable |> setComponent entityId 5

                    floatTable : Table Float
                    floatTable =
                        emptyTable |> setComponent entityId 5.5

                    stringTable : Table String
                    stringTable =
                        emptyTable |> setComponent entityId "yo"

                    func : EntityId -> Int -> Float -> String -> Tuple3 Int Float String
                    func _ int float string =
                        Tuple3 (int + 1) (float + 2.2) (string ++ "yo")
                in
                Expect.equal
                    (mapEntities3 func entityTable intTable floatTable stringTable)
                    (emptyTable |> setComponent entityId (Tuple3 6 7.7 "yoyo"))
            )
        ]
