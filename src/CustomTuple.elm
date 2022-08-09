module CustomTuple exposing
    ( Tuple2
    , Tuple3
    , Tuple4
    , Tuple5
    , toTuple2
    , toTuple3
    )


toTuple2 : a -> b -> Tuple2 a b
toTuple2 first second =
    { first = first, second = second }


toTuple3 : a -> b -> c -> Tuple3 a b c
toTuple3 first second third =
    { first = first, second = second, third = third }


toTuple4 : a -> b -> c -> d -> Tuple4 a b c d
toTuple4 first second third fourth =
    { first = first, second = second, third = third, fourth = fourth }


toTuple5 : a -> b -> c -> d -> e -> Tuple5 a b c d e
toTuple5 first second third fourth fifth =
    { first = first, second = second, third = third, fourth = fourth, fifth = fifth }


type alias Tuple2 a b =
    { first : a
    , second : b
    }


type alias Tuple3 a b c =
    { first : a
    , second : b
    , third : c
    }


type alias Tuple4 a b c d =
    { first : a
    , second : b
    , third : c
    , fourth : d
    }


type alias Tuple5 a b c d e =
    { first : a
    , second : b
    , third : c
    , fourth : d
    , fifth : e
    }
