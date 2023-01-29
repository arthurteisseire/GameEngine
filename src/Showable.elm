module Showable exposing (..)

import ComponentAI exposing (ComponentAI)
import ComponentAnimation exposing (ComponentAnimation)
import ComponentAttack exposing (ComponentAttack)
import ComponentDamage exposing (ComponentDamage)
import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentLife exposing (ComponentLife)
import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import ComponentTerrain exposing (ComponentTerrain)
import ComponentTurn exposing (ComponentTurn)
import ComponentVelocity exposing (ComponentVelocity)
import ComponentVisual exposing (ComponentVisual)
import Core.Table as Table exposing (Table)
import KeyboardInput
import Vector2 exposing (Vector2)


type alias Showable a =
    { toString : a -> String
    }


vector2 : { toString : (number -> String) -> Vector2 number -> String }
vector2 =
    { toString = Vector2.toString
    }


vectorFloat : Showable (Vector2 Float)
vectorFloat =
    { toString = vector2.toString String.fromFloat
    }


ai : Showable ComponentAI
ai =
    { toString = \_ -> "AI()"
    }


animation : Showable ComponentAnimation
animation =
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


attack : Showable ComponentAttack
attack =
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


damage : Showable ComponentDamage
damage =
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


keyboardInput : Showable ComponentKeyboardInput
keyboardInput =
    { toString =
        \keyboardInputA ->
            case keyboardInputA.key of
                Just key ->
                    "KeyboardInput(" ++ KeyboardInput.toString key ++ ")"

                Nothing ->
                    "KeyboardInput()"
    }


life : Showable ComponentLife
life =
    { toString =
        \lifeA ->
            "Life(healPoints = "
                ++ String.fromInt lifeA.healPoints
                ++ ")"
    }


player : Showable ComponentPlayer
player =
    { toString =
        \_ ->
            "ComponentPlayer()"
    }


position : Showable ComponentPosition
position =
    { toString = \positionA -> "Position(" ++ vectorFloat.toString positionA ++ ")"
    }


terrain : Showable ComponentTerrain
terrain =
    { toString =
        \terrainA ->
            "Terrain(dimensions="
                ++ Vector2.toString String.fromInt terrainA.dimensions
                ++ ", "
                ++ "sizeFactor="
                ++ String.fromInt terrainA.sizeFactor
                ++ ")"
    }


turn : Showable ComponentTurn
turn =
    { toString =
        \turnA ->
            "ComponentTurn(remainingTurns=" ++ String.fromInt turnA.remainingTurns ++ ")"
    }


velocity : Showable ComponentVelocity
velocity =
    { toString =
        \velocityA ->
            "Velocity(" ++ vectorFloat.toString velocityA ++ ")"
    }


visual : Showable ComponentVisual
visual =
    { toString =
        \visualA ->
            "Visual(position=(" ++ Vector2.vectorFloatToString visualA.position ++ "))"
    }


table : Showable a -> Showable (Table a)
table showable =
    { toString =
        \tableA ->
            Table.foldl
                (\_ component finalString -> finalString ++ showable.toString component)
                ""
                tableA
    }
