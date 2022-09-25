module SystemTriggerAttackAnimation exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentDamage exposing (ComponentDamage)
import EntityTable exposing (..)
import Vector2
import World exposing (World)


type alias InputComponents =
    { damage : ComponentDamage
    }


type alias OutputComponents =
    { animation : ComponentAnimation
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map
            (\animation ->
                let
                    inputComponents =
                        (InputComponents
                            |> from world.damageComponents
                        )
                            world.entities
                            |> removeInTable entityId

                    components =
                        triggerAnimation entityId inputComponents (OutputComponents animation)
                in
                { world
                    | animationComponents = insertComponent entityId components.animation world.animationComponents
                }
            )
            (getComponent entityId world.animationComponents)


triggerAnimation : EntityId -> Table InputComponents -> OutputComponents -> OutputComponents
triggerAnimation entityId damageTable { animation } =
    let
        allAnimations =
            foldlTable
                (\_ input list ->
                    let
                        damages =
                            List.filter (\dam -> dam.fromEntity == entityId) input.damage

                        animations =
                            List.map
                                (\dam ->
                                    ComponentAnimation.attackAnimation dam.fromDirection
                                )
                                damages
                    in
                    animations
                 --if List.filter (\input -> input.fromEntity == entityId) input.damage then
                 --    True :: list
                 --
                 --else
                 --    list
                )
                []
                damageTable

        anim =
            case List.head allAnimations of
                Just first ->
                    first

                Nothing ->
                    animation

        --if List.isEmpty touchedEntities then
        --    animation
        --
        --else
        --    ComponentAnimation.attackAnimation (Vector2.mul { x = 0.2, y = 0.2 })
    in
    { animation = anim
    }
