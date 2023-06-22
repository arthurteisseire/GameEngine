# GameEngine
Entity Component System (ECS) in Elm.\
This project is still work in progress but I'm pretty satisfied of the macro architecture.

## Motivation
I wanted to create small games in functional programming that people around me can play easily. Elm is perfect for that as it transpiles itself into javascript.
I started by creating the game without engine, just by iteration. I got stuck fastly, because reusing the code was a redundant question.
I didn't find any game engine in Elm that suited my cases. So, I started doing a small component/system engine.
My objective, is to keep it small and simple. It should be easy to improve your game but also the engine itself.
For now, the game engine is in Core directory but, I'll extract it into a library when my game will be more mature.

## Architecture
This is an Entity Component System (ECS) game engine.
- An Entity is no more than an id (an int for now but it will be a UUID or configurable).
- A Component is pure data without logic. Or, at least, helper functions.
Each component also contains it's debugger logic (toString and toVisualDebug).
Each component type is stored in a table (or dictionary) with EntityId as key and component as value. 
Systems query tables to get the wanted components.
- The system is the only way to modify entity components. And thus, game states.
The simplest one takes components in input, components in output and a function to modify them.
Each system takes a set of entity it wants to operate on. This allow filtering entities between system in an easy way (for streaming for example).

## Install & Run (todo: simplify with true builds)
- Install Elm: https://guide.elm-lang.org/install/elm.html.
- Go to the project directory and run "elm reactor".
- It will tell you to go to something like localhost:8080.
- You will be face of a project structure, browse to src/Main.elm.
- You can now play.

