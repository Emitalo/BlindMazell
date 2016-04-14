module Maze(Key, Door, Object, Maze, createKey, createDoor, openDoor, addFirstLeft) where

import Data.Maybe

data Key = Null | Key {key :: Integer}
	deriving (Show, Ord, Eq)

createKey :: Integer -> Key
createKey key = Key key

data Door = EmptyDoor | Door Key 
	deriving (Show, Ord, Eq)

createDoor :: Key -> Door
createDoor doorKey = Door doorKey

openDoor :: Door -> Key -> Bool 
openDoor EmptyDoor key = True
openDoor (Door dk) Null = False
openDoor (Door dk) key
	| dk == key = True
	| otherwise = False

data Object = NoObject | ObjectDoor Door | ObjectKey Key
	deriving (Show, Ord, Eq)

data Maze = NoExit | MazeEnd | Ambience{ object :: Object, father :: Maze, left :: Maze, right :: Maze } 
	deriving (Show, Ord, Eq)

addFirstLeft :: Maze -> Object -> Maze
addFirstLeft NoExit obj = Ambience obj NoExit NoExit NoExit
addFirstLeft maze obj
	| left maze == NoExit = Ambience (object maze) (father maze) (Ambience obj maze NoExit NoExit) (right maze)
	| left maze /= NoExit && right maze /= NoExit = Ambience (object maze) (father maze) (addFirstLeft (left maze) obj) (right maze)
	| right maze == NoExit = Ambience (object maze) (father maze) (left maze) (Ambience obj maze NoExit NoExit)
	| otherwise = Ambience (object maze) (father maze) (left maze) (addFirstLeft (right maze) obj)

data Player =  Player String [Key] Maze 
	deriving (Show, Ord, Eq)

createPlayer :: String -> Maze -> Player
createPlayer name maze = Player name [] maze

--isKey ObjectKey _ = True 

--walkLeft :: Player -> Maybe Player
--walkLeft (Player name bag (Ambience obj father left right)) = 
	
k = createKey 10
k2 = createKey 5
d = createDoor k

ok = ObjectKey k
od = ObjectDoor d
on = NoObject

r = openDoor d k
r1 = openDoor d k2

maze = addFirstLeft NoExit on
maze1 = addFirstLeft maze ok
maze2 = addFirstLeft maze1 on
maze3 = addFirstLeft maze2 od

player = createPlayer "Emilinda" maze3

t = father maze3
u = father (Ambience NoObject maze3 NoExit NoExit)