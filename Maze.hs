module Maze(Key, Door, Object, Maze, createKey, createDoor, openDoor, addFirstLeft) where

data Key = Null | Key {key :: Integer}
	deriving (Show, Ord, Eq)

createKey :: Integer -> Key
createKey key = Key key

data Door = EmptyDoor | Door {doorKey :: Key} 
	deriving (Show, Ord, Eq)

createDoor :: Key -> Door
createDoor doorKey = Door doorKey

openDoor :: Door -> Key -> Bool 
openDoor EmptyDoor key = True
openDoor (Door dk) Null = False
openDoor (Door dk) key
	| dk == key = True
	| otherwise = False

data Object = NoObject | ObjectDoor {objectDoor :: Door} | ObjectKey {objectKey :: Key}
	deriving (Show, Ord, Eq)

isKey :: Object -> Bool
isKey obj = 
	case obj of
		ObjectKey {} -> True
		otherwise -> False

isDoor :: Object -> Bool
isDoor obj = 
	case obj of
		ObjectDoor {} -> True
		otherwise -> False

toString :: Object -> String
toString object = 
	case object of
		NoObject {} -> "NoObject"
		ObjectKey {objectKey} -> "Key " ++ ( show (key objectKey))
		ObjectDoor {objectDoor} -> "Door" ++ (show (doorKey objectDoor))

data Maze = NoExit | MazeEnd | Ambience{ object :: Object, father :: Maze, left :: Maze, right :: Maze } 
	deriving (Show, Ord, Eq)

addFirstLeft :: Maze -> Object -> Maze
addFirstLeft NoExit obj = Ambience obj NoExit NoExit NoExit
addFirstLeft maze obj
	| left maze == NoExit = Ambience (object maze) (father maze) (Ambience obj maze NoExit NoExit) (right maze)
	| left maze /= NoExit && right maze /= NoExit = Ambience (object maze) (father maze) (addFirstLeft (left maze) obj) (right maze)
	| right maze == NoExit = Ambience (object maze) (father maze) (left maze) (Ambience obj maze NoExit NoExit)
	| otherwise = Ambience (object maze) (father maze) (left maze) (addFirstLeft (right maze) obj)

identLevel :: Integer -> String 
identLevel level 
	| level == 1 = "\t"
	| otherwise = "\t" ++ identLevel (level - 1)

getMazeLevel :: Maze -> Integer
getMazeLevel maze 
	| maze == NoExit = 0
	| otherwise = 1 + getMazeLevel (father maze)

printMaze :: Maze -> IO ()
printMaze NoExit = putStrLn "NoExit"
printMaze maze = putStrLn (printMazeAux (father maze) maze)

printMazeAux :: Maze -> Maze -> String
printMazeAux f NoExit = "\n" ++ identLevel level ++ "|NoExit"
	where level = ((getMazeLevel f) + 2)
printMazeAux f maze = ("\n" ++ identLevel level ++ "|" ++ (toString (object maze))) ++ (printMazeAux (father maze) (left maze)) ++ (printMazeAux (father maze) (right maze))
	where level = getMazeLevel maze

data Player =  Player {name :: String, bag :: [Key], curMaze :: Maze}
	deriving (Show, Ord, Eq)

createPlayer :: String -> Maze -> Player
createPlayer name maze = Player name [] maze


playerHasDoorKey :: Player -> Door -> Bool
playerHasDoorKey player door 
	| bag player == []  = False
	| head(bag player) == doorKey door = True
	| otherwise = playerHasDoorKey (Player (name player) (tail(bag player)) (curMaze player)) door

addToPlayerBag :: Player -> Key -> [Key]
addToPlayerBag player key = key : (bag player)

walkLeft :: Player -> (Player, String)
walkLeft player
	| leftMaze == NoExit = (player, "Nada a esquerda")
	| curMazeObj == NoObject = ((Player (name player) (bag player) leftMaze), "Voce foi para a esquerda")
	| isKey curMazeObj = ((Player (name player) (addToPlayerBag player (objectKey curMazeObj)) leftMaze), "Voce pegou uma chave")
	| isDoor curMazeObj && playerHasDoorKey player (objectDoor curMazeObj) = ((Player (name player) (bag player) leftMaze), "Voce abriu a porta e foi para a esquerda")
	| otherwise = (player, "Tem um porta aqui e voce nao tem a chave dessa porta.")
	where 
		curMazeObj = object (left (curMaze player))
		leftMaze = left (curMaze player)

walkRight :: Player -> (Player, String)
walkRight player
	| rightMaze == NoExit = (player, "Nada a direita")
	| curMazeObj == NoObject = ((Player (name player) (bag player) rightMaze), "Voce foi para a direita")
	| isKey curMazeObj = ((Player (name player) (addToPlayerBag player (objectKey curMazeObj)) rightMaze), "Voce pegou uma chave")
	| isDoor curMazeObj && playerHasDoorKey player (objectDoor curMazeObj) = ((Player (name player) (bag player) rightMaze), "Voce abriu a porta e foi para a direita")
	| otherwise = (player, "Tem um porta aqui e voce nao tem a chave dessa porta.")
	where 
		curMazeObj = object (right (curMaze player))
		rightMaze = right (curMaze player)

k = createKey 10
k2 = createKey 5
d = createDoor k
d2 = createDoor k2

ok = ObjectKey k
ok2 = ObjectKey k2
od = ObjectDoor d
od2 = ObjectDoor d2
on = NoObject

r = openDoor d k
r1 = openDoor d k2

maze = addFirstLeft NoExit on
maze1 = addFirstLeft maze ok
maze2 = addFirstLeft maze1 on
maze3 = addFirstLeft maze2 od
maze4 = addFirstLeft maze3 on
maze5 = addFirstLeft maze4 on
maze6 = addFirstLeft maze5 on
maze7 = addFirstLeft maze6 ok2
maze8 = addFirstLeft maze7 on
maze9 = addFirstLeft maze8 od2

player = createPlayer "Emilinda" maze3

player1 = walkLeft player
player2 = walkRight (fst player1)
player3 = walkLeft (fst player2)