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
import Core.EntityId exposing (EntityId)
import Core.EntitySet exposing (EntitySet)
import Core.Modifier exposing (Modifier)


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


foldl : (WorldTableOps -> result -> result) -> result -> result
foldl func result =
    result
        |> func (toWorldTableOps ComponentPosition.modifier)
        |> func (toWorldTableOps ComponentKeyboardInput.modifier)
        |> func (toWorldTableOps ComponentVelocity.modifier)
        |> func (toWorldTableOps ComponentLife.modifier)
        |> func (toWorldTableOps ComponentVisual.modifier)
        |> func (toWorldTableOps ComponentAttack.modifier)
        |> func (toWorldTableOps ComponentDamage.modifier)
        |> func (toWorldTableOps ComponentAnimation.modifier)
        |> func (toWorldTableOps ComponentTurn.modifier)
        |> func (toWorldTableOps ComponentTerrain.modifier)
        |> func (toWorldTableOps ComponentAI.modifier)
        |> func (toWorldTableOps ComponentPlayer.modifier)


toStrings : EntityId -> World -> List String
toStrings entityId world =
    foldl (\ops -> ops.toString world entityId) []


remove : EntityId -> World -> World
remove entityId world =
    foldl (\ops -> ops.remove entityId) world



-- Private


type alias WorldTableOps =
    { remove : EntityId -> World -> World
    , toString : World -> EntityId -> List String -> List String
    }


toWorldTableOps : Modifier (ComponentTable a) World -> WorldTableOps
toWorldTableOps modifier =
    { remove =
        \id currentWorld ->
            modifier.map (ComponentTable.remove id) currentWorld
    , toString =
        \world entityId strings ->
            strings
                ++ Maybe.withDefault
                    []
                    (Maybe.map
                        (\comp -> [ (ComponentTable.getOps (modifier.get world)).toString comp ])
                        (ComponentTable.get entityId (modifier.get world))
                    )
    }
