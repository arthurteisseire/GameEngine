module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Vector =
    { x : Float
    , y : Float
    }


type alias Line =
    { start : Vector
    , end : Vector
    }


type alias Intersection =
    { x : Float
    , y : Float
    , exists : Bool
    }


detectIntersection : Line -> Line -> Intersection
detectIntersection line1 line2 =
    let
        vector1 =
            { x = line1.end.x - line1.start.x, y = line1.end.y - line1.start.y }

        vector2 =
            { x = line2.end.x - line2.start.x, y = line2.end.y - line2.start.y }

        crossProduct =
            vector1.x * vector2.y - vector1.y * vector2.x
    in
    if crossProduct == 0 then
        { x = 0, y = 0, exists = False }

    else
        let
            thirdVector =
                { x = line2.start.x - line1.start.x, y = line2.start.y - line1.start.y }

            t =
                (thirdVector.x * vector2.y - thirdVector.y * vector2.x) / crossProduct

            u =
                (thirdVector.x * vector1.y - thirdVector.y * vector1.x) / crossProduct
        in
        if t >= 0 && t <= 1 && u >= 0 && u <= 1 then
            let
                intersectionX =
                    line1.start.x + t * vector1.x

                intersectionY =
                    line1.start.y + t * vector1.y
            in
            { x = intersectionX, y = intersectionY, exists = True }

        else
            { x = 0, y = 0, exists = False }


drawLine : Line -> Svg msg
drawLine line =
    Svg.line
        [ stroke "green"
        , strokeWidth "2"
        , x1 <| String.fromFloat line.start.x
        , y1 <| String.fromFloat line.start.y
        , x2 <| String.fromFloat line.end.x
        , y2 <| String.fromFloat line.end.y
        ]
        []


drawIntersection : Intersection -> Svg msg
drawIntersection intersection =
    if intersection.exists then
        let
            ax =
                intersection.x

            ay =
                intersection.y
        in
        g []
            [ circle
                [ cx <| String.fromFloat ax
                , cy <| String.fromFloat ay
                , r "5"
                , fill "red"
                ]
                []
            , Svg.line
                [ x <| String.fromFloat (ax + 5)
                , y <| String.fromFloat (ay - 5)
                , fontSize "12"
                , fontWeight "bold"
                , textAnchor "start"
                ]
                [ text "(Intersection)" ]
            ]

    else
        svg [] []


chart : Line -> Line -> Svg msg
chart line1 line2 =
    let
        intersection =
            detectIntersection line1 line2
    in
    svg [ width "400", height "400" ]
        [ drawLine line1
        , drawLine line2
        , drawIntersection intersection
        ]


main : Svg msg
main =
    let
        line1 =
            { start = { x = 0, y = 0 }, end = { x = 400, y = 400 } }

        line2 =
            { start = { x = 200, y = 0 }, end = { x = 200, y = 600 } }
    in
    chart line1 line2