module ComponentAttack exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier
import Vector2 exposing (Vector2)


type alias ComponentAttack =
    Maybe
        { from : Vector2 Float
        , to : Vector2 Float
        }


emptyTable : ComponentTable ComponentAttack
emptyTable =
    ComponentTable.empty
        { toString =
            \maybeAttack ->
                case maybeAttack of
                    Just attackA ->
                        "Attack(from="
                            ++ Vector2.vectorFloatToString attackA.from
                            ++ ", to="
                            ++ Vector2.vectorFloatToString attackA.to
                            ++ ")"

                    Nothing ->
                        "Attack()"
        }


modifier =
    Modifier.tableModifier
        { get = .attackComponents
        , set = \table world -> { world | attackComponents = table }
        }


identity : ComponentAttack
identity =
    Nothing
