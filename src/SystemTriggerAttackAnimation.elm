module SystemTriggerAttackAnimation exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentDamage exposing (ComponentDamage)
import EntityTable exposing (..)
import World exposing (..)


type alias OutputComponents =
    { animation : ComponentAnimation
    }


type alias InputComponents =
    { animation : ComponentAnimation
    }


type alias OtherComponents =
    { damage : ComponentDamage
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    updateComponentsWithOthers
        { func = triggerAnimation entityId
        , inputComponents =
            toInputComponents InputComponents
                |> withInput .animationComponents
        , otherComponents =
            select OtherComponents
                |> using world.entities
                |> remove entityId
                |> andFrom world.damageComponents
        , output =
            toOutputComponents
                |> withOutput animationComponent
        }
        entityId
        world


triggerAnimation : EntityId -> Table OtherComponents -> InputComponents -> OutputComponents
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
