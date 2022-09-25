module Vector2 exposing (..)


type alias Vector2 =
    { x : Float
    , y : Float
    }


identity : Vector2
identity =
    { x = 0, y = 0 }


eq : Vector2 -> Vector2 -> Bool
eq left right =
    left.x == right.x && left.y == right.y


isNull : Vector2 -> Bool
isNull vector =
    eq vector identity


add : Vector2 -> Vector2 -> Vector2
add =
    apply (+)


sub : Vector2 -> Vector2 -> Vector2
sub =
    apply (-)


div : Vector2 -> Vector2 -> Vector2
div =
    apply (/)


mul : Vector2 -> Vector2 -> Vector2
mul =
    apply (*)


apply : (Float -> Float -> Float) -> Vector2 -> Vector2 -> Vector2
apply func left right =
    { x = func left.x right.x
    , y = func left.y right.y
    }


toString : Vector2 -> String
toString vector =
    "V2(x=" ++ String.fromFloat vector.x ++ ", y=" ++ String.fromFloat vector.y ++ ")"
