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
import Core.EntityId exposing (EntityId)
import Core.EntitySet exposing (EntitySet)
import Core.Modifier as Modifier exposing (Modifier)
import Core.Table as Table exposing (Table)
import Event exposing (Msg)
import Html exposing (Html)


type alias World =
    { entities : EntitySet
    , keyboardInputComponents : Table ComponentKeyboardInput
    , positionComponents : Table ComponentPosition
    , velocityComponents : Table ComponentVelocity
    , lifeComponents : Table ComponentLife
    , visualComponents : Table ComponentVisual
    , attackComponents : Table ComponentAttack
    , damageComponents : Table ComponentDamage
    , animationComponents : Table ComponentAnimation
    , turnComponents : Table ComponentTurn
    , terrainComponents : Table ComponentTerrain
    , aiComponents : Table ComponentAI
    , playerComponents : Table ComponentPlayer
    , entityIdDebug : Maybe EntityId
    , isPause : Bool
    }



-- Table Modifiers


keyboardInputModifier : Modifier (Table ComponentKeyboardInput) World
keyboardInputModifier =
    Modifier.tableModifier
        { get = .keyboardInputComponents
        , set = \table world -> { world | keyboardInputComponents = table }
        }


positionModifier : Modifier (Table ComponentPosition) World
positionModifier =
    Modifier.tableModifier
        { get = .positionComponents
        , set = \table world -> { world | positionComponents = table }
        }


velocityModifier : Modifier (Table ComponentVelocity) World
velocityModifier =
    Modifier.tableModifier
        { get = .velocityComponents
        , set = \table world -> { world | velocityComponents = table }
        }


lifeModifier : Modifier (Table ComponentLife) World
lifeModifier =
    Modifier.tableModifier
        { get = .lifeComponents
        , set = \table world -> { world | lifeComponents = table }
        }


visualModifier : Modifier (Table ComponentVisual) World
visualModifier =
    Modifier.tableModifier
        { get = .visualComponents
        , set = \table world -> { world | visualComponents = table }
        }


attackModifier : Modifier (Table ComponentAttack) World
attackModifier =
    Modifier.tableModifier
        { get = .attackComponents
        , set = \table world -> { world | attackComponents = table }
        }


damageModifier : Modifier (Table ComponentDamage) World
damageModifier =
    Modifier.tableModifier
        { get = .damageComponents
        , set = \table world -> { world | damageComponents = table }
        }


animationModifier : Modifier (Table ComponentAnimation) World
animationModifier =
    Modifier.tableModifier
        { get = .animationComponents
        , set = \table world -> { world | animationComponents = table }
        }


turnModifier : Modifier (Table ComponentTurn) World
turnModifier =
    Modifier.tableModifier
        { get = .turnComponents
        , set = \table world -> { world | turnComponents = table }
        }


terrainModifier : Modifier (Table ComponentTerrain) World
terrainModifier =
    Modifier.tableModifier
        { get = .terrainComponents
        , set = \table world -> { world | terrainComponents = table }
        }


aiModifier : Modifier (Table ComponentAI) World
aiModifier =
    Modifier.tableModifier
        { get = .aiComponents
        , set = \table world -> { world | aiComponents = table }
        }


playerModifier : Modifier (Table ComponentPlayer) World
playerModifier =
    Modifier.tableModifier
        { get = .playerComponents
        , set = \table world -> { world | playerComponents = table }
        }
