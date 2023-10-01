module SystemTurn exposing (..)

import ComponentTurn exposing (ComponentTurn)
import Core.Component as Component
import Core.ComponentTable as ComponentTable
import Core.Context as Context
import Core.Modifier as Modifier


type alias Components =
    { turn : ComponentTurn
    }


updateEntity =
    Context.updateComponents
        { func = playTurn
        , inputComponents =
            Component.select Components
                |> Component.join ComponentTurn.modifier.get
        , output =
            Modifier.select
                |> ComponentTable.joinModifier ( ComponentTurn.modifier.map, .turn )
        }


playTurn : Components -> Components
playTurn { turn } =
    if turn.remainingTurns == 0 then
        { turn =
            { turn | remainingTurns = turn.turnsToPlay }
        }

    else
        { turn =
            { turn | remainingTurns = turn.remainingTurns - 1 }
        }
