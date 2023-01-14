module SystemTakeDamage exposing (..)

import ComponentAttack exposing (ComponentAttack)
import ComponentDamage exposing (ComponentDamage)
import ComponentPosition exposing (ComponentPosition)
import EntityTable exposing (..)
import Vector2
import World exposing (..)


type alias OutputComponents =
    { damage : ComponentDamage
    }


type alias InputComponents =
    { position : ComponentPosition
    }


type alias OtherComponents =
    { attack : ComponentAttack
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    updateComponentsWithOthers
        { func = takeDamage
        , inputComponents =
            toInputComponents InputComponents
                |> withInput .positionComponents
        , otherComponents =
            select OtherComponents
                |> using world.entities
                |> remove entityId
                |> andFrom world.attackComponents
        , output =
            toOutputComponents
                |> withOutput damageComponent
        }
        entityId
        world


takeDamage : Table OtherComponents -> InputComponents -> OutputComponents
takeDamage attackTable { position } =
    let
        updatedDamage =
            foldlTable
                (\entityId input damages ->
                    case input.attack of
                        Just attack ->
                            let
                                fromDirection =
                                    Vector2.sub attack.to position.currentPos
                            in
                            if Vector2.isNull fromDirection then
                                { fromEntity = entityId
                                , fromDirection = Vector2.sub attack.to attack.from
                                , points = 1
                                }
                                    :: damages

                            else
                                damages

                        Nothing ->
                            damages
                )
                []
                attackTable
    in
    { damage = updatedDamage
    }
