module SystemAttack exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentAttack exposing (ComponentAttack)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import Vector2
import World exposing (..)


type alias OutputComponents =
    { attack : ComponentAttack
    , animation : ComponentAnimation
    }


type alias InputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    , animation : ComponentAnimation
    }


type alias OtherComponents =
    { position : ComponentPosition
    }


updateEntity : EntityId -> World -> World
updateEntity =
    updateComponentsWithOthers
        { func = velocityAttack
        , inputComponents =
            toInputComponents InputComponents
                |> withInput .positionComponents
                |> withInput .velocityComponents
                |> withInput .animationComponents
        , otherComponents =
            select OtherComponents
                |> using .entities
                |> andFrom .positionComponents
        , output =
            toOutputComponents
                |> withOutput attackComponent
                |> withOutput animationComponent
        }


velocityAttack : Table OtherComponents -> InputComponents -> OutputComponents
velocityAttack others { position, velocity, animation } =
    let
        attackPos =
            Vector2.add position velocity
    in
    if velocity /= ComponentVelocity.identity && hasValueInTableIf (\pos other -> pos == other.position) attackPos others then
        { attack =
            Just
                { from = position
                , to = Vector2.add position velocity
                }
        , animation =
            ComponentAnimation.attackAnimation velocity
        }

    else
        { attack = Nothing
        , animation = animation
        }


clear : EntityId -> World -> World
clear entityId world =
    { world | attackComponents = updateComponent entityId Nothing world.attackComponents }
