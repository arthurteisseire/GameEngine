module SystemAnimation exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentPosition exposing (ComponentPosition)
import ComponentVisual exposing (ComponentVisual)
import EntityTable exposing (..)
import World exposing (..)


type alias OutputComponents =
    { visual : ComponentVisual
    , animation : ComponentAnimation
    }


type alias InputComponents =
    { visual : ComponentVisual
    , animation : ComponentAnimation
    , position : ComponentPosition
    }


updateEntity : Float -> EntityId -> World -> World
updateEntity dt =
    updateComponents
        { func = animate dt
        , inputComponents =
            toInputComponents InputComponents
                |> withInput .visualComponents
                |> withInput .animationComponents
                |> withInput .positionComponents
        , output =
            toOutputComponents
                |> withOutput visualComponent
                |> withOutput animationComponent
        }


animate : Float -> InputComponents -> OutputComponents
animate dt { visual, animation, position } =
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
                }

            else
                { visual = { visual | position = position }
                , animation = Nothing
                }

        Nothing ->
            { visual = visual
            , animation = animation
            }
