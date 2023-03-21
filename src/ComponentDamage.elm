module ComponentDamage exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier
import Vector2 exposing (Vector2)


type alias ComponentDamage =
    List
        { fromDirection : Vector2 Float
        , points : Int
        }


emptyTable : ComponentTable ComponentDamage
emptyTable =
    ComponentTable.empty
        { toString =
            \damageA ->
                "ComponentDamage("
                    ++ List.foldl
                        (\currentDamage finalString ->
                            finalString
                                ++ "fromDirection="
                                ++ Vector2.vectorFloatToString currentDamage.fromDirection
                                ++ ", points="
                                ++ String.fromInt currentDamage.points
                        )
                        ""
                        damageA
                    ++ ")"
        }


modifier =
    Modifier.tableModifier
        { get = .damageComponents
        , set = \table world -> { world | damageComponents = table }
        }


identity : ComponentDamage
identity =
    []
