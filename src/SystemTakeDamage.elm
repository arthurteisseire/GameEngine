module SystemTakeDamage exposing (..)

import ComponentAttack exposing (ComponentAttack)
import ComponentDamage exposing (ComponentDamage)
import ComponentPosition exposing (ComponentPosition)
import Core.Component as Component
import Core.ComponentTable as ComponentTable
import Core.Context as Context
import Core.Modifier as Modifier
import Core.Table as Table exposing (Table)
import Vector2


type alias OutputComponents =
    { damage : ComponentDamage
    }


type alias InputComponents =
    { position : ComponentPosition
    }


type alias OtherComponents =
    { attack : ComponentAttack
    }


updateEntity =
    Context.updateComponentsWithOthers
        { func = takeDamage
        , inputComponents =
            Component.select InputComponents
                |> Component.join .positionComponents
        , otherComponents =
            Context.select OtherComponents
                |> Context.fromEntities .entities
                |> Context.innerJoin ComponentAttack.modifier.get
        , output =
            Modifier.select
                |> ComponentTable.joinModifier ( ComponentDamage.modifier.map, .damage )
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
