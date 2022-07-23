module EntityTableTests exposing (..)

import Dict
import EntityTable exposing (..)
import Expect exposing (..)
import Fuzz exposing (int, list)
import Test exposing (..)


suite : Test
suite =
    describe "Entity Table tests"
        [ test "splitTables"
            (\_ ->
                let
                    tableComponent2 =
                        emptyTable
                            |> insertInTable 1 { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }

                    expectedTable2 =
                        { tableA =
                            emptyTable
                                |> insertInTable 1 { x = 1, y = 2 }
                        , tableB =
                            emptyTable
                                |> insertInTable 1 { x = 3, y = 4 }
                        }
                in
                Expect.equal (splitTable2 tableComponent2) expectedTable2
            )
        , test "mergeTables"
            (\_ ->
                let
                    actualTable2 =
                        { tableA =
                            emptyTable
                                |> insertInTable 1 { x = 1, y = 2 }
                                |> insertInTable 2 { x = 1, y = 2 }
                        , tableB =
                            emptyTable
                                |> insertInTable 1 { x = 3, y = 4 }
                        }

                    expectedTableComponent2 =
                        emptyTable
                            |> insertInTable 1 { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }
                in
                Expect.equal (intersectTable2 actualTable2) expectedTableComponent2
            )
        , fuzz2
            (list int)
            (list int)
            "merge then split"
            (\xs ys ->
                let
                    tables =
                        { tableA = Table (Dict.fromList (List.indexedMap (\idx x -> ( idx, x )) xs))
                        , tableB = Table (Dict.fromList (List.indexedMap (\idx y -> ( idx, y )) ys))
                        }
                in
                Expect.equal tables (unionTable2 (splitTable2 (intersectTable2 tables)) tables)
            )
        , fuzz2
            (list int)
            (list int)
            "update 2 tables"
            (\xs ys ->
                let
                    tables =
                        { tableA = Table (Dict.fromList (List.indexedMap (\idx x -> ( idx, x )) xs))
                        , tableB = Table (Dict.fromList (List.indexedMap (\idx y -> ( idx, y )) ys))
                        }
                in
                Expect.equal tables (update2Tables (\tableComp2 -> tableComp2) tables)
            )
        ]

