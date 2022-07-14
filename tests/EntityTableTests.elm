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
                        emptyComponentTable
                            |> insertInTable 1 { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }

                    expectedTable2 =
                        { a =
                            emptyComponentTable
                                |> insertInTable 1 { x = 1, y = 2 }
                        , b =
                            emptyComponentTable
                                |> insertInTable 1 { x = 3, y = 4 }
                        }
                in
                Expect.equal (splitTable tableComponent2) expectedTable2
            )
        , test "mergeTables"
            (\_ ->
                let
                    actualTable2 =
                        { a =
                            emptyComponentTable
                                |> insertInTable 1 { x = 1, y = 2 }
                                |> insertInTable 2 { x = 1, y = 2 }
                        , b =
                            emptyComponentTable
                                |> insertInTable 1 { x = 3, y = 4 }
                        }

                    expectedTableComponent2 =
                        emptyComponentTable
                            |> insertInTable 1 { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }
                in
                Expect.equal (table2ToMatchingEntities actualTable2) expectedTableComponent2
            )
        , fuzz2
            (list int)
            (list int)
            "merge then split"
            (\xs ys ->
                let
                    tables =
                        { a = Table (Dict.fromList (List.indexedMap (\idx x -> ( idx, x )) xs))
                        , b = Table (Dict.fromList (List.indexedMap (\idx y -> ( idx, y )) ys))
                        }
                in
                Expect.equal tables (unionTables (splitTable (table2ToMatchingEntities tables)) tables)
            )
        , fuzz2
            (list int)
            (list int)
            "update 2 tables"
            (\xs ys ->
                let
                    tables =
                        { a = Table (Dict.fromList (List.indexedMap (\idx x -> ( idx, x )) xs))
                        , b = Table (Dict.fromList (List.indexedMap (\idx y -> ( idx, y )) ys))
                        }
                in
                Expect.equal tables (update2Tables (\tableComp2 -> tableComp2) tables)
            )
        ]

