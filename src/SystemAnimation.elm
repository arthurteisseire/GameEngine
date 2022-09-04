module SystemAnimation exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentPosition exposing (ComponentPosition)
import EntityTable exposing (..)
import World exposing (World)


type alias Components =
    { position : ComponentPosition
    , animation : ComponentAnimation
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map2
            (\position animation ->
                let
                    components =
                        animate entityId (Components position animation)
                in
                { world
                    | positionComponents = updateComponent entityId components.position world.positionComponents
                    , animationComponents = updateComponent entityId components.animation world.animationComponents
                }
            )
            (getComponent entityId world.positionComponents)
            (getComponent entityId world.animationComponents)


animate : EntityId -> Components -> Components
animate entityId { position, animation } =
    case animation of
        Just anim ->
            { position =
                { x = position.x + anim.offsetX
                , y = position.y + anim.offsetY
                }
            , animation = animation
            }

        Nothing ->
            { position = position
            , animation = animation
            }
