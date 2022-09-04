module Vector2 exposing (..)


type alias Vector2 =
    { x : Float
    , y : Float
    }


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
