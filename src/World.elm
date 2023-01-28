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
import Core.Modifier as Modifier
import Core.Table exposing (Table)


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



-- Below are a way to link (Tables to World, Component to Table)


keyboardInputComponent =
    ( Modifier.tableModifier
        { get = .keyboardInputComponents
        , set = \table world -> { world | keyboardInputComponents = table }
        }
    , .keyboardInput
    )


positionComponent =
    ( Modifier.tableModifier
        { get = .positionComponents
        , set = \table world -> { world | positionComponents = table }
        }
    , .position
    )


velocityComponent =
    ( Modifier.tableModifier
        { get = .velocityComponents
        , set = \table world -> { world | velocityComponents = table }
        }
    , .velocity
    )


lifeComponent =
    ( Modifier.tableModifier
        { get = .lifeComponents
        , set = \table world -> { world | lifeComponents = table }
        }
    , .life
    )


visualComponent =
    ( Modifier.tableModifier
        { get = .visualComponents
        , set = \table world -> { world | visualComponents = table }
        }
    , .visual
    )


attackComponent =
    ( Modifier.tableModifier
        { get = .attackComponents
        , set = \table world -> { world | attackComponents = table }
        }
    , .attack
    )


damageComponent =
    ( Modifier.tableModifier
        { get = .damageComponents
        , set = \table world -> { world | damageComponents = table }
        }
    , .damage
    )


animationComponent =
    ( Modifier.tableModifier
        { get = .animationComponents
        , set = \table world -> { world | animationComponents = table }
        }
    , .animation
    )


turnComponent =
    ( Modifier.tableModifier
        { get = .turnComponents
        , set = \table world -> { world | turnComponents = table }
        }
    , .turn
    )


terrainComponent =
    ( Modifier.tableModifier
        { get = .terrainComponents
        , set = \table world -> { world | terrainComponents = table }
        }
    , .terrain
    )


aiComponent =
    ( Modifier.tableModifier
        { get = .aiComponents
        , set = \table world -> { world | aiComponents = table }
        }
    , .ai
    )


playerComponent =
    ( Modifier.tableModifier
        { get = .playerComponents
        , set = \table world -> { world | playerComponents = table }
        }
    , .player
    )
