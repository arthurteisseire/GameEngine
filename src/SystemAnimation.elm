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
updateEntity dt entityId world =
    updateComponentsNew
        { db = world
        , entityId = entityId
        , func = animate dt entityId
        , inputComponents =
            Just InputComponents
                |> withComponent entityId world.visualComponents
                |> withComponent entityId world.animationComponents
                |> withComponent entityId world.positionComponents
        , output =
            update2ComponentsNew
                visualComponent
                animationComponent
        }


animate : Float -> EntityId -> InputComponents -> OutputComponents
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
                }

            else
                { visual = visual
                , animation = Nothing
                }

        Nothing ->
            { visual = visual
            , animation = animation
            }
