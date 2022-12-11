module SystemAnimation exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentPosition exposing (ComponentPosition)
import ComponentVisual exposing (ComponentVisual)
import EntityTable exposing (..)
import World exposing (..)


type alias Components =
    { visual : ComponentVisual
    , animation : ComponentAnimation
    , position : ComponentPosition
    }


updateEntity : Float -> EntityId -> World -> World
updateEntity dt =
    update3Components
        (animate dt)
        Components
        visualComponent
        animationComponent
        positionComponent


animate : Float -> EntityId -> Components -> Components
animate dt _ { visual, animation, position } =
    case animation of
        Just anim ->
            let
                timeLeft =
                    anim.timeLeft - dt
            in
            if timeLeft > 0 then
                { visual =
                    { visual
                        | position =
                            { x = visual.position.x + ((anim.offset.x * dt) / anim.duration)
                            , y = visual.position.y + ((anim.offset.y * dt) / anim.duration)
                            }
                    }
                , animation =
                    Just
                        { duration = anim.duration
                        , timeLeft = timeLeft
                        , offset = anim.offset
                        }
                , position = position
                }

            else
                { visual = { visual | position = position.currentPos }
                , animation = Nothing
                , position = position
                }

        Nothing ->
            { visual = { visual | position = position.currentPos }
            , animation = animation
            , position = position
            }
