module Invaders where

import Window

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type UserInput = {}

userInput : Signal UserInput
userInput = constant {}

type Input = { timeDelta:Float, userInput:UserInput }



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}
(gameWidth, gameHeight) = (800, 600)
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)

type Position obj = {obj | x:Float, y:Float}
type Ship = Position {}
type Enemy = Position {}
type GameState = {ship: Ship, enemies: [Enemy]}

defaultGame : GameState
defaultGame = {ship={x=0, y=-halfHeight + 30}, enemies=defaultEnemies}

defaultEnemies : [Enemy]
defaultEnemies =
  let enemyPosition i = {x=-halfWidth + 30 + i * 60, y=halfHeight - 30}
   in map enemyPosition [0..9]


{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame {timeDelta,userInput} gameState = gameState



{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

displayObj : Form -> Position a -> Form
displayObj form obj = move (obj.x, obj.y) form

displayShip = displayObj (filled white (rect 30 30))
displayEnemy = displayObj (filled red (rect 30 30))

display : (Int,Int) -> GameState -> Element
display (w,h) {ship, enemies} = container w h middle <|
                            collage gameWidth gameHeight ([
                              filled black (rect gameWidth gameHeight),
                              displayShip ship
                            ] ++ map displayEnemy enemies)



{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta = fps 30
input = sampleOn delta (lift2 Input delta userInput)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState
