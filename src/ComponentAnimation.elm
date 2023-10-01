module ComponentAnimation exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier exposing (Modifier)
import Vector2 exposing (Vector2)


type alias ComponentAnimation =
    Maybe
        { duration : Float
        , timeLeft : Float
        , offset : Vector2 Float
        }


emptyTable : ComponentTable ComponentAnimation
emptyTable =
    ComponentTable.empty
        { toString =
            \maybeAnimation ->
                let
                    content =
                        case maybeAnimation of
                            Just animationA ->
                                "duration="
                                    ++ String.fromFloat animationA.duration
                                    ++ ", timeLeft="
                                    ++ String.fromFloat animationA.timeLeft
                                    ++ ", offset="
                                    ++ Vector2.vectorFloatToString animationA.offset

                            Nothing ->
                                ""
                in
                "ComponentAnimation(" ++ content ++ ")"
        }


modifier =
    Modifier.init
        { get = .animationComponents
        , set = \table world -> { world | animationComponents = table }
        }


identity : ComponentAnimation
identity =
    Nothing


attackAnimation : Vector2 Float -> ComponentAnimation
attackAnimation offset =
    let
        duration =
            0.1
    in
    Just
        { duration = duration
        , timeLeft = duration
        , offset = offset
        }
