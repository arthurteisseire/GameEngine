module SystemClearKeyboardInputs exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import EntityTable exposing (..)
import World exposing (World)


updateWorld : World -> World
updateWorld world =
    { world | keyboardInputComponents = mapEntities1 (\_ _ -> { key = Nothing }) world.entities world.keyboardInputComponents }
