module SystemTurn exposing (..)

import ComponentTurn exposing (ComponentTurn)
import EntityTable exposing (..)
import World exposing (World)


type alias Components =
    { turn : ComponentTurn
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map
            (\turn ->
                let
                    components =
                        playTurn entityId (Components turn)
                in
                { world
                    | turnComponents = insertComponent entityId components.turn world.turnComponents
                }
            )
            (getComponent entityId world.turnComponents)


playTurn : EntityId -> Components -> Components
playTurn _ components =
    let
        turn =
            components.turn
    in
    if components.turn.remainingTurns == 0 then
        { components | turn = { turn | remainingTurns = components.turn.turnsToPlay } }

    else
        { components | turn = { turn | remainingTurns = components.turn.remainingTurns - 1 } }
