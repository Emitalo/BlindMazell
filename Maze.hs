module Maze(Key, Door, Object, Maze, createKey, createDoor, openDoor, addFirstLeft, createPlayer) where

data Key = Null | Key Integer
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

data Maze = NoExit | Ambience Object Maze Maze Maze
	deriving (Show, Ord, Eq)

addFirstLeft :: Maze -> Object -> Maze
addFirstLeft NoExit obj = Ambience obj NoExit NoExit NoExit
addFirstLeft (Ambience o father left right) obj
	| left == NoExit = Ambience o father (Ambience obj f NoExit NoExit) right
	| left /= NoExit && right /= NoExit = Ambience o father (addFirstLeft left obj) right
	| right == NoExit = Ambience o father left (Ambience obj f NoExit NoExit)
	| otherwise = Ambience o father left (addFirstLeft right obj)
	where f = (Ambience o father left right)

data Player =  String [Key] Maze 
	deriving (Show, Ord, Eq)

--createPlayer :: String -> Key -> Maze -> Player
--createPlayer name list maze = name list maze

--walk 

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
