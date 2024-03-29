module World exposing (..)

import ComponentAI exposing (ComponentAI)
import ComponentAnimation exposing (ComponentAnimation)
import ComponentAttack exposing (ComponentAttack)
import ComponentDamage exposing (ComponentDamage)
import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentLife exposing (ComponentLife)
import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import ComponentTerrain exposing (ComponentTerrain)
import ComponentTurn exposing (ComponentTurn)
import ComponentVelocity exposing (ComponentVelocity)
import ComponentVisual exposing (ComponentVisual)
import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Context exposing (ContextOperations)
import Core.EntityId exposing (EntityId)
import Core.EntitySet exposing (EntitySet)
import KeyboardInput exposing (Key)


type alias World =
    { entities : EntitySet
    , keyboardInputComponents : ComponentTable ComponentKeyboardInput
    , positionComponents : ComponentTable ComponentPosition
    , velocityComponents : ComponentTable ComponentVelocity
    , lifeComponents : ComponentTable ComponentLife
    , visualComponents : ComponentTable ComponentVisual
    , attackComponents : ComponentTable ComponentAttack
    , damageComponents : ComponentTable ComponentDamage
    , animationComponents : ComponentTable ComponentAnimation
    , turnComponents : ComponentTable ComponentTurn
    , terrainComponents : ComponentTable ComponentTerrain
    , aiComponents : ComponentTable ComponentAI
    , playerComponents : ComponentTable ComponentPlayer
    , entityIdDebug : Maybe EntityId
    , isPause : Bool
    }


type Msg
    = Tick Float
    | TogglePause
    | NextFrame
    | KeyBoardInput Key
    | DiscardMsg
    | DisplayDebug EntityId
    | HideDebug


foldl : (ComponentTable.Ops World -> result -> result) -> result -> result
foldl func result =
    result
        |> func (ComponentTable.toOps ComponentPosition.modifier)
        |> func (ComponentTable.toOps ComponentKeyboardInput.modifier)
        |> func (ComponentTable.toOps ComponentVelocity.modifier)
        |> func (ComponentTable.toOps ComponentLife.modifier)
        |> func (ComponentTable.toOps ComponentVisual.modifier)
        |> func (ComponentTable.toOps ComponentAttack.modifier)
        |> func (ComponentTable.toOps ComponentDamage.modifier)
        |> func (ComponentTable.toOps ComponentAnimation.modifier)
        |> func (ComponentTable.toOps ComponentTurn.modifier)
        |> func (ComponentTable.toOps ComponentTerrain.modifier)
        |> func (ComponentTable.toOps ComponentAI.modifier)
        |> func (ComponentTable.toOps ComponentPlayer.modifier)


operations : ContextOperations World
operations =
    { toStrings = \entityId world -> foldl (\ops -> ops.toString world entityId) []
    , remove = \entityId world -> foldl (\ops -> ops.remove entityId) world
    }


toStrings : EntityId -> World -> List String
toStrings entityId world =
    foldl (\ops -> ops.toString world entityId) []


remove : EntityId -> World -> World
remove entityId world =
    foldl (\ops -> ops.remove entityId) world


visualMsgToWorldMsg : EntityId -> ComponentVisual.VisualMsg -> Msg
visualMsgToWorldMsg entityId visualMsg =
    case visualMsg of
        ComponentVisual.Clicked ->
            DisplayDebug entityId

        ComponentVisual.None ->
            DiscardMsg
