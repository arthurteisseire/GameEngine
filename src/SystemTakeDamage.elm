module SystemTakeDamage exposing (..)

import ComponentAttack exposing (ComponentAttack)
import ComponentDamage exposing (ComponentDamage)
import ComponentPosition exposing (ComponentPosition)
import EntityTable exposing (..)
import Vector2
import World exposing (World)


type alias InputComponents =
    { attack : ComponentAttack
    }


type alias OutputComponents =
    { position : ComponentPosition
    , damage : ComponentDamage
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map2
            (\position life ->
                let
                    inputComponents =
                        (InputComponents
                            |> from world.attackComponents
                        )
                            world.entities
                            |> removeInTable entityId

                    components =
                        takeDamage entityId inputComponents (OutputComponents position life)
                in
                { world
                    | positionComponents = insertComponent entityId components.position world.positionComponents
                    , damageComponents = insertComponent entityId components.damage world.damageComponents
                }
            )
            (getComponent entityId world.positionComponents)
            (getComponent entityId world.damageComponents)


takeDamage : EntityId -> Table InputComponents -> OutputComponents -> OutputComponents
takeDamage _ attackTable { position, damage } =
    let
        updatedDamage =
            foldlTable
                (\entityId input damages ->
                    case input.attack of
                        Just attack ->
                            let
                                fromDirection =
                                    Vector2.sub attack.to position
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
