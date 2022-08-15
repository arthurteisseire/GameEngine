module SystemClearKeyboardInputs exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import EntityTable exposing (..)
import World exposing (World)


updateWorld : World -> World
updateWorld world =
    { world
        | keyboardInputComponents =
            mapEntities (\_ _ -> { key = Nothing }) world.keyboardInputComponents world.entities
    }
