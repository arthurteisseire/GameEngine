module DictHelpersTests exposing (..)

import Dict exposing (Dict)
import DictHelpers
import Expect
import Test exposing (Test, describe, test)


type alias User =
    { age : Int
    }


type alias Model =
    { users : Dict Int User
    , residents : Dict Int String
    }


type alias UserResident =
    { user : User
    , resident : String
    }


suite : Test
suite =
    describe "DictHelpers tests"
        [ test "join 2 empty dict"
            (\_ ->
                let
                    model =
                        { users = Dict.empty
                        , residents = Dict.empty
                        }
                in
                Expect.equal
                    0
                    (DictHelpers.select UserResident
                        |> DictHelpers.from model.users
                        |> DictHelpers.innerJoin model.residents
                        |> Dict.size
                    )
            )
        , test "join 2 dict"
            (\_ ->
                let
                    model =
                        { users =
                            Dict.fromList
                                [ ( 1, { age = 10 } )
                                , ( 2, { age = 18 } )
                                ]
                        , residents =
                            Dict.fromList
                                [ ( 1, "test" )
                                ]
                        }
                in
                Expect.equal
                    1
                    (DictHelpers.select UserResident
                        |> DictHelpers.from (Dict.filter (\_ user -> user.age < 15) model.users)
                        |> DictHelpers.innerJoin model.residents
                        |> Dict.size
                    )
            )
        , test "andThen"
            (\_ ->
                let
                    model =
                        { users =
                            Dict.fromList
                                [ ( 1, { age = 10 } )
                                , ( 2, { age = 18 } )
                                ]
                        , residents =
                            Dict.fromList
                                [ ( 1, "test" )
                                ]
                        }
                in
                Expect.equal
                    1
                    (DictHelpers.select UserResident
                        |> DictHelpers.from (Dict.filter (\_ user -> user.age < 15) model.users)
                        |> DictHelpers.andThen (\_ _ -> model.residents)
                        --|> DictHelpers.innerJoin model.residents
                        |> Dict.size
                    )
            )
        ]


size : Model -> Int
size model =
    let
        locals : Dict Int String
        locals =
            Dict.filter
                (\_ city -> city == "Test")
                model.residents
    in
    DictHelpers.select UserResident
        |> DictHelpers.from model.users
        |> DictHelpers.innerJoin model.residents
        |> Dict.size
