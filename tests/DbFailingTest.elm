module DbFailingTest exposing (..)


notImplemented =
    True



----- TEST
--
--
--type alias User =
--    { userId : Int
--    , age : Int
--    }
--
--
--type alias Model =
--    { users : Dict Int User
--    , residents : Dict Int String
--    }
--
--
--size : Model -> Int
--size model =
--    let
--        locals =
--            Dict.filter
--                (\userId city -> city == "Test")
--                model.residents
--    in
--    model.users
--        |> Dict.intersect locals
--        |> Dict.size
