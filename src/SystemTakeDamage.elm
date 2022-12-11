module SystemTakeDamage exposing (..)

import ComponentAttack exposing (ComponentAttack)
import ComponentDamage exposing (ComponentDamage)
import ComponentPosition exposing (ComponentPosition)
import EntityTable exposing (..)
import Vector2
import World exposing (..)


type alias InputComponents =
    { attack : ComponentAttack
    }


type alias OutputComponents =
    { position : ComponentPosition
    , damage : ComponentDamage
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    update2Components
        (takeDamage
            (select InputComponents
                |> using world.entities
                |> remove entityId
                |> andFrom world.attackComponents
            )
        )
        OutputComponents
        positionComponent
        damageComponent
        entityId
        world


takeDamage : Table InputComponents -> EntityId -> OutputComponents -> OutputComponents
takeDamage attackTable _ { position, damage } =
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
    { position = position
    , damage = updatedDamage
    }
