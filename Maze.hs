module Maze(Key, Door, Object (NoObject, ObjectDoor, ObjectKey, MazeEnd, Hole, Bear, Sword, Flashlight), Maze (NoExit), Player (Winner, Loser),
	createKey, createDoor, openDoor, addFirstLeft, createPlayer, walkLeft, walkRight, printMaze, addInRight, addInRightLeft, 
	playerHasAFlashlight, showNextSteps, showObjectInNextSteps, isSword, isKey, isDoor, isEnd, isBear, isHole, isFlashlight, deleteFlashlight) where

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

data Object = NoObject | ObjectDoor {objectDoor :: Door} | ObjectKey {objectKey :: Key} | Hole | Bear | Flashlight | Sword | MazeEnd
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

isEnd :: Object -> Bool
isEnd obj = 
	case obj of
		MazeEnd {} -> True
		otherwise -> False


isHole :: Object -> Bool
isHole obj = 
	case obj of
		Hole {} -> True
		otherwise -> False

isBear :: Object -> Bool
isBear obj = 
	case obj of
		Bear {} -> True
		otherwise -> False

isFlashlight:: Object -> Bool
isFlashlight obj = 
	case obj of
		Flashlight {} -> True
		otherwise -> False

isSword :: Object -> Bool
isSword obj = 
	case obj of
		Sword {} -> True
		otherwise -> False


toString :: Object -> String
toString object = 
	case object of
		NoObject {} -> "NoObject"
		ObjectKey {objectKey} -> "Key " ++ ( show (key objectKey))
		ObjectDoor {objectDoor} -> "Door" ++ (show (doorKey objectDoor))
		MazeEnd {} -> "END"
		Hole {} -> "Hole"
		Bear {} -> "Bear"
		Flashlight {} -> "Flashlight"
		Sword {} -> "Sword"

data Maze = NoExit | Ambience{ object :: Object, father :: Maze, left :: Maze, right :: Maze } 
	deriving (Show, Ord, Eq)

addFirstLeft :: Maze -> Object -> Maze
addFirstLeft NoExit obj = Ambience obj NoExit NoExit NoExit
addFirstLeft maze obj
	| left maze == NoExit = Ambience (object maze) (father maze) (Ambience obj maze NoExit NoExit) (right maze)
	| left maze /= NoExit && right maze /= NoExit = Ambience (object maze) (father maze) (addFirstLeft (left maze) obj) (right maze)
	| right maze == NoExit = Ambience (object maze) (father maze) (left maze) (Ambience obj maze NoExit NoExit)
	| otherwise = Ambience (object maze) (father maze) (left maze) (addFirstLeft (right maze) obj)

addInRight :: Maze -> Object -> Maze
addInRight NoExit obj = Ambience obj NoExit NoExit NoExit
addInRight maze obj
	| right maze == NoExit = Ambience (object maze) (father maze) (left maze) (Ambience obj maze NoExit NoExit) 
	| right maze /= NoExit = Ambience (object maze) (father maze) (left maze) (addInRight (right maze) obj)
	| otherwise = Ambience (object maze) (father maze) (left maze) (addInRight (right maze) obj)

addInRightLeft :: Maze -> Object -> Maze
addInRightLeft NoExit obj = Ambience obj NoExit NoExit NoExit
addInRightLeft maze obj
	| left maze == NoExit = Ambience (object maze) (father maze) (Ambience obj maze NoExit NoExit) (right maze) 
	| right maze /= NoExit = Ambience (object maze) (father maze) (left maze) (addInRightLeft (right maze) obj)
	| otherwise = Ambience (object maze) (father maze) (left maze) (addInRightLeft (right maze) obj)

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

data Player =  Player {name :: String, bag :: [Object], curMaze :: Maze} | Winner | Loser
	deriving (Show, Ord, Eq)

createPlayer :: String -> Maze -> Player
createPlayer name maze = Player name [] maze

playerHasDoorKey :: Player -> Door -> Bool
playerHasDoorKey player door 
	| bag player == []  = False
	| isKey object && objectKey object == doorKey door = True
	| otherwise = playerHasDoorKey (Player (name player) (tail(bag player)) (curMaze player)) door
	where
		object = head (bag player)

playerHasASword :: Player -> Bool
playerHasASword player 
	| (bag player) == [] = False
	| head(bag player) == Sword = True
	| otherwise = playerHasASword (Player (name player) (tail(bag player)) (curMaze player)) 

playerHasAFlashlight :: Player -> Bool
playerHasAFlashlight player 
	| (bag player) == [] = False
	| head(bag player) == Flashlight = True
	| otherwise = playerHasAFlashlight (Player (name player) (tail(bag player)) (curMaze player)) 

addToPlayerBag :: Player -> Object -> [Object]
addToPlayerBag player object = object : (bag player)

walkLeft :: Player -> (Player, String)
walkLeft player
	| leftMaze == NoExit = (player, "Nada a esquerda")
	| curMazeObj == NoObject = ((Player (name player) (bag player) leftMaze), "Voce foi para a esquerda")
	| isKey curMazeObj = ((Player (name player) (addToPlayerBag player curMazeObj) leftMaze), "Voce pegou uma chave")
	| isDoor curMazeObj && playerHasDoorKey player (objectDoor curMazeObj) = ((Player (name player) (bag player) leftMaze), "Voce abriu a porta e foi para a esquerda")
	| isHole curMazeObj = (Loser, "Voce caiu em um buraco! Fim do jogo.")
	| isBear curMazeObj && playerHasASword player = ((Player (name player) (bag player) leftMaze), "Voce encontrou um urso, mas voce tinha uma espada e o matou, depois voce foi para a esquerda")
	| isBear curMazeObj && (not (playerHasASword player)) = (Loser, "Ghrrr!! Voce encontrou um urso, mas voce não tinha uma espada e morreu. Fim do jogo.")
	| isFlashlight curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) leftMaze), "Voce encontrou uma lanterna, voce pode usar apenas uma vez para enxergar o que tem nos seus possíveis caminhos. \n Voce pegou a lanterna e foi para a esquerda.")
	| isSword curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) leftMaze), "Voce encontrou uma espada e foi para a esquerda.")
	| isEnd curMazeObj = (Winner, "Voce saiu do labirinto! Fim do jogo.")
	| otherwise = (player, "Tem um porta aqui e voce nao tem a chave dessa porta.")
	where 
		curMazeObj = object (left (curMaze player))
		leftMaze = left (curMaze player)

walkRight :: Player -> (Player, String)
walkRight player
	| rightMaze == NoExit = (player, "Nada a direita")
	| curMazeObj == NoObject = ((Player (name player) (bag player) rightMaze), "Voce foi para a direita")
	| isKey curMazeObj = ((Player (name player) (addToPlayerBag player curMazeObj) rightMaze), "Voce pegou uma chave")
	| isDoor curMazeObj && playerHasDoorKey player (objectDoor curMazeObj) = ((Player (name player) (bag player) rightMaze), "Voce abriu a porta e foi para a direita")
	| isEnd curMazeObj = (Winner, "Você saiu do labirinto! Fim do jogo.")
	| isHole curMazeObj = (Loser, "Voce caiu em um buraco! Fim do jogo.")
	| isBear curMazeObj && playerHasASword player = ((Player (name player) (bag player) rightMaze), "Voce encontrou um urso, mas voce tinha uma espada e o matou, depois voce foi para a direita")
	| isBear curMazeObj && (not (playerHasASword player)) = (Loser, "GHRR \n Voce encontrou um urso, mas voce não tinha uma espada e morreu. Fim do jogo.")
	| isFlashlight curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) rightMaze), "Voce encontrou uma lanterna, voce pode usar apenas uma vez para enxergar o que tem nos seus possíveis caminhos. \n Voce pegou a lanterna e foi para a direita.")
	| isSword curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) rightMaze), "Voce encontrou uma espada e foi para a direita.")
	| otherwise = (player, "Tem um porta aqui e voce nao tem a chave dessa porta.")
	where 
		curMazeObj = object (right (curMaze player))
		rightMaze = right (curMaze player)

showNextSteps :: Player -> (Bool, Bool)
showNextSteps player
	| leftMaze /= NoExit && rightMaze /= NoExit = (True, True)
	| leftMaze /= NoExit && rightMaze == NoExit = (True, False)
	| leftMaze == NoExit && rightMaze /= NoExit = (False, True)
	| leftMaze == NoExit && rightMaze == NoExit = (False, False)
	where
		rightMaze = right (curMaze player)
		leftMaze = left (curMaze player)

showObjectInNextSteps :: Player -> (Object, Object)
showObjectInNextSteps player = ((object leftMaze), (object rightMaze)) 
	where
	rightMaze = right (curMaze player)
	leftMaze = left (curMaze player)

deleteFlashlight :: Player -> Player
deleteFlashlight player 
	| isFlashlight curObj = newPlayer
	| otherwise = Player (name player) newBag (curMaze player)
	where
		curObj = head(bag player)
		newBag =  (curObj) : bag (deleteFlashlight (newPlayer))
		newPlayer = (Player (name player) (tail(bag player)) (curMaze player))

-- Creating scenario I
--k = createKey 10
--k2 = createKey 5
--d = createDoor k

---- Create objects

--k = createKey 10
--k2 = createKey 5
--d = createDoor k
--d2 = createDoor k2

--ok = ObjectKey k
--ok2 = ObjectKey k2
--od = ObjectDoor d
--od2 = ObjectDoor d2
--on = NoObject

k = createKey 10
k2 = createKey 5
d = createDoor k
d2 = createDoor k2

ok = ObjectKey k
ok2 = ObjectKey k2
od = ObjectDoor d
od2 = ObjectDoor d2
on = NoObject


--r = openDoor d k
--r1 = openDoor d k2

--maze = addFirstLeft NoExit on
--maze1 = addFirstLeft maze ok
--maze2 = addFirstLeft maze1 on
--maze3 = addFirstLeft maze2 od
--maze4 = addFirstLeft maze3 on
--maze5 = addFirstLeft maze4 on
--maze6 = addFirstLeft maze5 on
--maze7 = addFirstLeft maze6 ok2
--maze8 = addFirstLeft maze7 on
--maze9 = addFirstLeft maze8 od2

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


--player = createPlayer "Emilinda" maze3

--player1 = walkLeft player
--player2 = walkRight (fst player1)
--player3 = walkLeft (fst player2)
