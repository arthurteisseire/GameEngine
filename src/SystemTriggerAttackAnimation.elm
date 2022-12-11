module SystemTriggerAttackAnimation exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentDamage exposing (ComponentDamage)
import EntityTable exposing (..)
import World exposing (..)


type alias InputComponents =
    { damage : ComponentDamage
    }


type alias OutputComponents =
    { animation : ComponentAnimation
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    update1Component
        (triggerAnimation
            (select InputComponents
                |> using world.entities
                |> remove entityId
                |> andFrom world.damageComponents
            )
        )
        OutputComponents
        animationComponent
        entityId
        world


triggerAnimation : Table InputComponents -> EntityId -> OutputComponents -> OutputComponents
triggerAnimation damageTable entityId { animation } =
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
                    list ++ animations
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
