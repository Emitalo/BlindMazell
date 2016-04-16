module Maze(Key, Door, Object (NoObject, ObjectDoor, ObjectKey, MazeEnd, Hole, Bear, Sword, Flashlight), Maze (NoExit), Player (Winner, Loser),
	createKey, createDoor, addFirstLeft, createPlayer, walkLeft, walkRight, walkBack, printMaze, addInRight, addInRightLeft, 
	playerHasAFlashlight, showNextSteps, showObjectInNextSteps, isSword, isKey, isDoor, isEnd, isBear, isHole, isFlashlight, deleteFlashlight) where

data Key = Null | Key {key :: Integer}
	deriving (Show, Ord, Eq)

createKey :: Integer -> Key
createKey key = Key key

data Door = EmptyDoor | Door {doorKey :: Key} 
	deriving (Show, Ord, Eq)

createDoor :: Key -> Door
createDoor doorKey = Door doorKey

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

data Player =  Player {name :: String, bag :: [Object], curMaze :: Maze, moves :: [Maze]} | Winner | Loser
	deriving (Show, Ord, Eq)

createPlayer :: String -> Maze -> Player
createPlayer name maze = Player name [] maze []

playerHasDoorKey :: Player -> Door -> Bool
playerHasDoorKey player door 
	| bag player == []  = False
	| isKey object && objectKey object == doorKey door = True
	| otherwise = playerHasDoorKey (Player (name player) (tail(bag player)) (curMaze player) (moves player)) door
	where
		object = head (bag player)

playerHasASword :: Player -> Bool
playerHasASword player 
	| (bag player) == [] = False
	| head(bag player) == Sword = True
	| otherwise = playerHasASword (Player (name player) (tail(bag player)) (curMaze player) (moves player)) 

playerHasAFlashlight :: Player -> Bool
playerHasAFlashlight player 
	| (bag player) == [] = False
	| head(bag player) == Flashlight = True
	| otherwise = playerHasAFlashlight (Player (name player) (tail(bag player)) (curMaze player) (moves player)) 

addToPlayerBag :: Player -> Object -> [Object]
addToPlayerBag player object = object : (bag player)

walkLeft :: Player -> (Player, String)
walkLeft player
	| leftMaze == NoExit = (player, "Nada a esquerda")
	| curMazeObj == NoObject = ((Player (name player) (bag player) leftMaze pMoves), "Voce foi para a esquerda")
	| isKey curMazeObj = ((Player (name player) (addToPlayerBag player curMazeObj) leftMaze pMoves), "Voce pegou uma chave")
	| isDoor curMazeObj && playerHasDoorKey player (objectDoor curMazeObj) = ((Player (name player) (bag player) leftMaze pMoves), "Voce abriu a porta e foi para a esquerda")
	| isHole curMazeObj = (Loser, "Voce caiu em um buraco! Fim do jogo.")
	| isBear curMazeObj && playerHasASword player = ((Player (name player) (bag player) leftMaze pMoves), "Voce encontrou um urso, mas voce tinha uma espada e o matou, depois voce foi para a esquerda")
	| isBear curMazeObj && (not (playerHasASword player)) = (Loser, "Ghrrr!! Voce encontrou um urso, mas voce não tinha uma espada e morreu. Fim do jogo.")
	| isFlashlight curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) leftMaze pMoves), "Voce encontrou uma lanterna, voce pode usar apenas uma vez para enxergar o que tem nos seus possíveis caminhos. \n Voce pegou a lanterna e foi para a esquerda.")
	| isSword curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) leftMaze pMoves), "Voce encontrou uma espada e foi para a esquerda.")
	| isEnd curMazeObj = (Winner, "Voce saiu do labirinto! Fim do jogo.")
	| otherwise = (player, "Tem um porta aqui e voce nao tem a chave dessa porta.")
	where 
		curMazeObj = object (left (curMaze player))
		leftMaze = left (curMaze player)
		pMoves = (curMaze player) : (moves player)

walkRight :: Player -> (Player, String)
walkRight player
	| rightMaze == NoExit = (player, "Nada a direita")
	| curMazeObj == NoObject = ((Player (name player) (bag player) rightMaze pMoves), "Voce foi para a direita")
	| isKey curMazeObj = ((Player (name player) (addToPlayerBag player curMazeObj) rightMaze pMoves), "Voce pegou uma chave")
	| isDoor curMazeObj && playerHasDoorKey player (objectDoor curMazeObj) = ((Player (name player) (bag player) rightMaze pMoves), "Voce abriu a porta e foi para a direita")
	| isEnd curMazeObj = (Winner, "Você saiu do labirinto! Fim do jogo.")
	| isHole curMazeObj = (Loser, "Voce caiu em um buraco! Fim do jogo.")
	| isBear curMazeObj && playerHasASword player = ((Player (name player) (bag player) rightMaze pMoves), "Voce encontrou um urso, mas voce tinha uma espada e o matou, depois voce foi para a direita")
	| isBear curMazeObj && (not (playerHasASword player)) = (Loser, "GHRR \n Voce encontrou um urso, mas voce não tinha uma espada e morreu. Fim do jogo.")
	| isFlashlight curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) rightMaze pMoves), "Voce encontrou uma lanterna, voce pode usar apenas uma vez para enxergar o que tem nos seus possíveis caminhos. \n Voce pegou a lanterna e foi para a direita.")
	| isSword curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) rightMaze pMoves), "Voce encontrou uma espada e foi para a direita.")
	| otherwise = (player, "Tem um porta aqui e voce nao tem a chave dessa porta.")
	where
		curMazeObj = object (right (curMaze player))
		rightMaze = right (curMaze player)
		pMoves = (curMaze player) : (moves player)

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
	| otherwise = Player (name player) newBag (curMaze player) (moves player)
	where
		curObj = head(bag player)
		newBag =  (curObj) : bag (deleteFlashlight (newPlayer))
		newPlayer = (Player (name player) (tail(bag player)) (curMaze player) (moves player))

previousMaze :: Player -> Maze
previousMaze player = head (moves player)

walkBack :: Player -> (Player, String)
walkBack player
	| moves player == [] = (player, "Voce esta no comeco do labirinto, nao da mais pra voltar!")
	| otherwise = (backingPlayer, "1, 2, 3 uno passito para tras...")
	where backingPlayer = (Player (name player) (bag player) (previousMaze player) (tail (moves player)) )

--playerWhereabouts :: Player -> IO ()
--playerWhereabouts player 
--	| father(curMaze player) == NoExit = putStrLn "Comeco do Labirinto"
--	| otherwise = putStrLn ("Voce esta nesse labirinto ...") >> printMaze (father (curMaze player))

