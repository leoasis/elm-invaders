module Invaders where

import Signal exposing ((<~), (~))
import Time
import Random
import Keyboard
import Window
import Graphics.Element
import Graphics.Collage
import Color

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

------------------------------------------------------------------------------}

type alias UserInput = { direction: Int, shooting: Bool }

userInput : Signal UserInput
userInput = UserInput <~ Signal.map .x Keyboard.arrows
                       ~ Keyboard.space

type alias Input = { timeDelta:Float, userInput:UserInput }



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

------------------------------------------------------------------------------}
(gameWidth, gameHeight) = (800, 600)
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
shipSpeed = 1/6
shipMissileSpeed = 1/4
enemyMissileSpeed = 1/8

-- Components
type alias Position obj = {obj | x:Float, y:Float}
type alias Size obj = {obj | w:Float, h:Float}
type alias VelocityX obj = {obj | vx:Float}
type alias VelocityY obj = {obj | vy:Float}

-- Entities
type alias Ship = Position (Size (VelocityX {}))
type alias ShipMissile = Position (Size (VelocityY {}))
type alias Enemy = Position (Size {})
type alias EnemyMissile = Position (Size (VelocityY {}))

type alias GameState = {
  ship: Ship,
  enemies: List Enemy,
  enemyMissiles: List EnemyMissile,
  shipMissile: Maybe ShipMissile,
  seed: Random.Seed }

defaultGame : GameState
defaultGame = {
  ship=defaultShip,
  enemies=defaultEnemies,
  enemyMissiles=[],
  shipMissile=Nothing,
  seed=Random.initialSeed 0 }

defaultShip : Ship
defaultShip = {x=0, y=-halfHeight + 30, w=30, h=30, vx=0}

defaultEnemies : List Enemy
defaultEnemies =
  let enemyPosition i = {x=-halfWidth + 30 + i * 60, y=halfHeight - 30, w=30, h=30}
   in List.map enemyPosition [0..9]


{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

------------------------------------------------------------------------------}

flatMapMaybe : (a -> Maybe b) -> Maybe a -> Maybe b
flatMapMaybe f m = case (Maybe.map f m) of
  Just val -> val
  Nothing -> Nothing

stepGame : Input -> GameState -> GameState
stepGame {timeDelta, userInput} ({ship, shipMissile, enemies, enemyMissiles, seed} as game) =
      -- First move the entities
  let (random, seed') = Random.generate (Random.float 0 1) game.seed
      ship' = moveShip userInput.direction timeDelta ship

      shipMissile' = case shipMissile of
        Just missile -> Just <| moveShipMissile timeDelta missile
        Nothing -> if userInput.shooting then Just (shootShipMissile ship) else Nothing

      enemies' = List.map (moveEnemy timeDelta) enemies
      enemyMissiles' =
        List.map (moveEnemyMissile timeDelta) enemyMissiles
        ++ List.filterMap (tryShootEnemyMissile timeDelta random enemyMissiles) enemies'

      -- Then detect collissions
      enemies'' = case shipMissile' of
        Just missile -> List.filterMap (destroyIf (shouldDestroyEnemy missile)) enemies'
        Nothing      -> enemies'

      shipMissile'' = flatMapMaybe (destroyIf (shouldDestroyMissile enemies)) <| shipMissile'
  in
    { game | ship <- ship', shipMissile <- shipMissile'', enemies <- enemies'', enemyMissiles <- enemyMissiles', seed <- seed' }

collide : Position (Size a) -> Position (Size b) -> Bool
collide a b =
  let top i = i.y + i.h / 2
      bottom i = i.y - i.h / 2
      left i = i.x - i.w / 2
      right i = i.x + i.w / 2
      collidesHorizontally = ((left a) < (left b) && (left b) < (right a)) || ((left b) < (left a) && (left a) < (right b))
      collidesVertically = ((bottom a) < (bottom b) && (bottom b) < (top a)) || ((bottom b) < (bottom a) && (bottom a) < (top b))
  in collidesHorizontally && collidesVertically

collidesWithAny : List (Position (Size a)) -> Position (Size b) -> Bool
collidesWithAny entities entity = List.any (collide entity) entities

outOfBounds : Position a -> Bool
outOfBounds entity =
  (entity.y > halfHeight) ||
  (entity.y < -halfHeight) ||
  (entity.x > halfWidth) ||
  (entity.x < -halfWidth)

destroyIf : (a -> Bool) -> a -> Maybe a
destroyIf test entity = if test entity then Nothing else Just entity

-- Enemy logic
moveEnemy : Time.Time -> Enemy -> Enemy
moveEnemy t enemy = enemy

shouldDestroyEnemy : ShipMissile -> Enemy -> Bool
shouldDestroyEnemy missile = collide missile

tryShootEnemyMissile : Time.Time -> Float -> List EnemyMissile -> Enemy -> Maybe EnemyMissile
tryShootEnemyMissile t random missiles enemy =
  if random > 0.8 then
    Just {x=enemy.x, y=enemy.y,vy=enemyMissileSpeed,w=10,h=20}
  else
    Nothing

-- Enemy missile logic
moveEnemyMissile : Time.Time -> EnemyMissile -> EnemyMissile
moveEnemyMissile t ({y, vy, h} as missile) =
  let vy' = enemyMissileSpeed
      y'  = y - (vy') * t
  in {missile | y <- y', vy <- vy'}


-- Ship missile logic
moveShipMissile : Time.Time -> ShipMissile -> ShipMissile
moveShipMissile t ({y, vy, h} as missile) =
  let vy' = shipMissileSpeed
      y'  = y + (vy') * t
  in {missile | y <- y', vy <- vy'}

shouldDestroyMissile : List Enemy -> ShipMissile -> Bool
shouldDestroyMissile enemies missile = (outOfBounds missile) || (collidesWithAny enemies missile)

-- Ship logic
moveShip : Int -> Time.Time -> Ship -> Ship
moveShip direction t ({x, vx, w} as ship) =
  let vx' = toFloat direction * shipSpeed
      x'  = clamp (w-halfWidth) (halfWidth-w) (x + (vx') * t)
  in {ship | x <- x', vx <- vx'}

shootShipMissile : Ship -> ShipMissile
shootShipMissile {x,y} = {x=x, y=y,vy=shipMissileSpeed,w=10,h=20}

{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

------------------------------------------------------------------------------}

displayInPosition : Graphics.Collage.Form -> Position a -> Graphics.Collage.Form
displayInPosition form obj = Graphics.Collage.move (obj.x, obj.y) form

shipShape ship = (Graphics.Collage.filled Color.white (Graphics.Collage.rect ship.w ship.h))
displayShip ship = displayInPosition (shipShape ship) ship

shipMissileShape missile = (Graphics.Collage.filled Color.white (Graphics.Collage.rect missile.w missile.h))
displayShipMissile missile = displayInPosition (shipMissileShape missile) missile

enemyShape enemy = (Graphics.Collage.filled Color.red (Graphics.Collage.rect enemy.w enemy.h))
displayEnemy enemy = displayInPosition (enemyShape enemy) enemy

enemyMissileShape missile = (Graphics.Collage.filled Color.green (Graphics.Collage.rect missile.w missile.h))
displayEnemyMissile missile = displayInPosition (enemyMissileShape missile) missile

wrapInList : a -> List a
wrapInList a = [a]

display : (Int,Int) -> GameState -> Graphics.Element.Element
display (w,h) {ship, enemies, shipMissile, enemyMissiles} = Graphics.Element.container w h Graphics.Element.middle <|
                            Graphics.Collage.collage gameWidth gameHeight (
                              [
                                Graphics.Collage.filled Color.black (Graphics.Collage.rect gameWidth gameHeight),
                                displayShip ship
                              ]
                              ++ List.map displayEnemy enemies
                              ++ Maybe.withDefault [] (Maybe.map (displayShipMissile >> wrapInList) shipMissile)
                              ++ List.map displayEnemyMissile enemyMissiles
                            )



{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta = Time.fps 30
input = Signal.sampleOn delta (Signal.map2 Input delta userInput)

gameState = Signal.foldp stepGame defaultGame input

main = Signal.map2 display Window.dimensions gameState
