module SystemTakeDamage exposing (..)

import ComponentAttack exposing (ComponentAttack)
import ComponentDamage exposing (ComponentDamage)
import ComponentPosition exposing (ComponentPosition)
import Core.Component as Component
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.Modifier as Modifier
import Core.Table as Table exposing (Table)
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
updateEntity =
    Db.updateComponentsWithOthers
        { func = takeDamage
        , inputComponents =
            Component.select InputComponents
                |> Component.join .positionComponents
        , otherComponents =
            Db.select OtherComponents
                |> Db.fromEntities .entities
                |> Db.innerJoin ComponentAttack.modifier.get
        , output =
            Modifier.select
                |> Modifier.join ( ComponentDamage.modifier.map, .damage )
        }


takeDamage : Table OtherComponents -> InputComponents -> OutputComponents
takeDamage attackTable { position } =
    let
        updatedDamage =
            Table.foldl
                (\_ input damages ->
                    case input.attack of
                        Just attack ->
                            let
                                fromDirection =
                                    Vector2.sub attack.to position
                            in
                            if Vector2.isNull fromDirection then
                                { fromDirection = Vector2.sub attack.to attack.from
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
