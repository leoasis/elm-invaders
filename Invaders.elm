module Invaders where

import Window
import Keyboard
import Maybe

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type UserInput = {direction: Int, shooting: Bool}

userInput : Signal UserInput
userInput = UserInput <~ lift .x Keyboard.arrows
                       ~ Keyboard.space

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
shipSpeed = 1/6
shipMissileSpeed = 1/4

-- Components
type Position obj = {obj | x:Float, y:Float}
type Size obj = {obj | w:Float, h:Float}
type VelocityX obj = {obj | vx:Float}
type VelocityY obj = {obj | vy:Float}

-- Entities
type Ship = Position (Size (VelocityX {}))
type ShipMissile = Position (Size (VelocityY {}))
type Enemy = Position (Size {})

type GameState = {ship: Ship, enemies: [Enemy], shipMissile: Maybe ShipMissile}

defaultGame : GameState
defaultGame = {ship=defaultShip, enemies=defaultEnemies, shipMissile=Nothing}

defaultShip : Ship
defaultShip = {x=0, y=-halfHeight + 30, w=30, h=30, vx=0}

defaultEnemies : [Enemy]
defaultEnemies =
  let enemyPosition i = {x=-halfWidth + 30 + i * 60, y=halfHeight - 30, w=30, h=30}
   in map enemyPosition [0..9]


{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

flatMapMaybe : (a -> Maybe b) -> Maybe a -> Maybe b
flatMapMaybe f m = case (Maybe.map f m) of
  Just val -> val
  Nothing -> Nothing

stepGame : Input -> GameState -> GameState
stepGame {timeDelta, userInput} ({ship, shipMissile, enemies} as game) =
      -- First move the entities
  let ship' = moveShip userInput.direction timeDelta ship

      shipMissile' = case shipMissile of
        Just missile -> Just <| moveMissile timeDelta missile
        Nothing -> if userInput.shooting then Just (shootShipMissile ship) else Nothing

      enemies' = map (moveEnemy timeDelta) enemies

      -- Then detect collissions
      enemies'' = case shipMissile' of
        Just missile -> filterMap (destroyIf (shouldDestroyEnemy missile)) enemies'
        Nothing      -> enemies'

      shipMissile'' = flatMapMaybe (destroyIf (shouldDestroyMissile enemies)) <| shipMissile'
  in
    { game | ship <- ship', shipMissile <- shipMissile'', enemies <- enemies'' }

shootShipMissile : Ship -> ShipMissile
shootShipMissile {x,y} = {x=x, y=y,vy=shipMissileSpeed,w=10,h=20}

collide : Position (Size a) -> Position (Size b) -> Bool
collide a b =
  let top i = i.y + i.h / 2
      bottom i = i.y - i.h / 2
      left i = i.x - i.w / 2
      right i = i.x + i.w / 2
      collidesHorizontally = ((left a) < (left b) && (left b) < (right a)) || ((left b) < (left a) && (left a) < (right b))
      collidesVertically = ((bottom a) < (bottom b) && (bottom b) < (top a)) || ((bottom b) < (bottom a) && (bottom a) < (top b))
  in collidesHorizontally && collidesVertically

collidesWithAny : [Position (Size a)] -> Position (Size b) -> Bool
collidesWithAny entities entity = any (collide entity) entities

outOfBounds : Position a -> Bool
outOfBounds entity =
  (entity.y > halfHeight) ||
  (entity.y < -halfHeight) ||
  (entity.x > halfWidth) ||
  (entity.x < -halfWidth)

destroyIf : (a -> Bool) -> a -> Maybe a
destroyIf test entity = if test entity then Nothing else Just entity

-- Enemies logic
moveEnemy : Time -> Enemy -> Enemy
moveEnemy t enemy = enemy

shouldDestroyEnemy : ShipMissile -> Enemy -> Bool
shouldDestroyEnemy missile = collide missile

-- Ship missile logic
moveMissile : Time -> ShipMissile -> ShipMissile
moveMissile t ({y, vy, h} as missile) =
  let vy' = shipMissileSpeed
      y'  = y + (vy') * t
  in {missile | y <- y', vy <- vy'}

shouldDestroyMissile : [Enemy] -> ShipMissile -> Bool
shouldDestroyMissile enemies missile = (outOfBounds missile) || (collidesWithAny enemies missile)

-- Ship logic
moveShip : Int -> Time -> Ship -> Ship
moveShip direction t ({x, vx, w} as ship) =
  let vx' = toFloat direction * shipSpeed
      x'  = clamp (w-halfWidth) (halfWidth-w) (x + (vx') * t)
  in {ship | x <- x', vx <- vx'}

{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

displayInPosition : Form -> Position a -> Form
displayInPosition form obj = move (obj.x, obj.y) form

shipShape ship = (filled white (rect ship.w ship.h))
displayShip ship = displayInPosition (shipShape ship) ship

shipMissileShape missile = (filled white (rect missile.w missile.h))
displayShipMissile missile = displayInPosition (shipMissileShape missile) missile

enemyShape enemy = (filled red (rect enemy.w enemy.h))
displayEnemy enemy = displayInPosition (enemyShape enemy) enemy

wrapInList : a -> [a]
wrapInList a = [a]

display : (Int,Int) -> GameState -> Element
display (w,h) {ship, enemies, shipMissile} = container w h middle <|
                            collage gameWidth gameHeight ([
                              filled black (rect gameWidth gameHeight),
                              displayShip ship
                            ] ++ map displayEnemy enemies ++ Maybe.maybe [] (displayShipMissile >> wrapInList) shipMissile)



{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta = fps 30
input = sampleOn delta (lift2 Input delta userInput)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState
