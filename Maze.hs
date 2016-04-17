module Maze(Key, Door, Object (NoObject, ObjectDoor, ObjectKey, MazeEnd, Hole, Bear, Sword, Flashlight), Maze (NoExit), Player (Winner, Loser),
	addFirstLeft, createScenario, createPlayer, walkLeft, walkRight, walkBack, printMaze, addInRight, addInRightLeft, 
	playerHasAFlashlight, showNextSteps, showObjectInNextSteps, isSword, isKey, isDoor, isEnd, isBear, isHole, isFlashlight, deleteFlashlight) where

import Objects(Key, Door(doorKey), Object (NoObject, ObjectDoor, objectDoor, ObjectKey, objectKey, MazeEnd, Hole, Bear, Sword, Flashlight),  
	createKey, createDoor, toString, isSword, isKey, isDoor, isEnd, isBear, isHole, isFlashlight)

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
printMaze NoExit = putStrLn "Sem saida"
printMaze maze = putStrLn (printMazeAux (father maze) maze)

printMazeAux :: Maze -> Maze -> String
printMazeAux f NoExit = "\n" ++ identLevel level ++ "|Sem saida"
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
	| leftMaze == NoExit = (player, "Nada a esquerda, volte!")
	| curMazeObj == NoObject = ((Player (name player) (bag player) leftMaze pMoves), "Voce foi para a esquerda")
	| isKey curMazeObj = ((Player (name player) (addToPlayerBag player curMazeObj) leftMaze pMoves), "Voce pegou uma chave")
	| isDoor curMazeObj && playerHasDoorKey player (objectDoor curMazeObj) = ((Player (name player) (bag player) leftMaze pMoves), "Voce abriu a porta e foi para a esquerda")
	| isHole curMazeObj = (Loser, "Voce caiu em um buraco! Fim do jogo.")
	| isBear curMazeObj && playerHasASword player = ((Player (name player) (bag player) leftMaze pMoves), "Voce encontrou um urso, mas voce tinha uma espada e o matou, depois voce foi para a esquerda")
	| isBear curMazeObj && (not (playerHasASword player)) = (Loser, "Ghrrr!! Voce encontrou um urso, mas voce não tinha uma espada e morreu. Fim do jogo.")
	| isFlashlight curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) leftMaze pMoves), "Voce encontrou uma lanterna. Voce pode usar apenas uma vez para enxergar o que tem nos seus possíveis caminhos. \n Voce pegou a lanterna e foi para a esquerda.")
	| isSword curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) leftMaze pMoves), "Voce encontrou uma espada e foi para a esquerda.")
	| isEnd curMazeObj = (Winner, "Voce saiu do labirinto! Fim do jogo.")
	| otherwise = (player, "Tem um porta aqui e voce nao tem a chave dessa porta.")
	where 
		curMazeObj = object (left (curMaze player))
		leftMaze = left (curMaze player)
		pMoves = (curMaze player) : (moves player)

walkRight :: Player -> (Player, String)
walkRight player
	| rightMaze == NoExit = (player, "Nada a direita, volte!")
	| curMazeObj == NoObject = ((Player (name player) (bag player) rightMaze pMoves), "Voce foi para a direita")
	| isKey curMazeObj = ((Player (name player) (addToPlayerBag player curMazeObj) rightMaze pMoves), "Voce pegou uma chave")
	| isDoor curMazeObj && playerHasDoorKey player (objectDoor curMazeObj) = ((Player (name player) (bag player) rightMaze pMoves), "Voce abriu a porta e foi para a direita")
	| isEnd curMazeObj = (Winner, "Você saiu do labirinto! Fim do jogo.")
	| isHole curMazeObj = (Loser, "Voce caiu em um buraco! Fim do jogo.")
	| isBear curMazeObj && playerHasASword player = ((Player (name player) (bag player) rightMaze pMoves), "Voce encontrou um urso, mas voce tinha uma espada e o matou, depois voce foi para a direita")
	| isBear curMazeObj && (not (playerHasASword player)) = (Loser, "Ghrrr!! Voce encontrou um urso, mas voce não tinha uma espada e morreu. Fim do jogo.")
	| isFlashlight curMazeObj = ((Player (name player) (addToPlayerBag player (curMazeObj)) rightMaze pMoves), "Voce encontrou uma lanterna. Voce pode usar apenas uma vez para enxergar o que tem nos seus possíveis caminhos. \n Voce pegou a lanterna e foi para a direita.")
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
	| otherwise = (backingPlayer, "Voce voltou")
	where backingPlayer = (Player (name player) (bag player) (previousMaze player) (tail (moves player)) )

createScenario :: Maze
createScenario = do
	
	-- Create objects
	let k = createKey 10
	let k2 = createKey 5
	let d = createDoor k
	let d2 = createDoor k2

	let ok = ObjectKey k
	let ok2 = ObjectKey k2
	let od = ObjectDoor d
	let od2 = ObjectDoor d2
	let on = NoObject

	let hl = Hole
	let b = Bear
	let f = Flashlight
	let s = Sword

	-- Create mazes
	let maze = addFirstLeft NoExit on
	let maze1 = addFirstLeft maze on
	let maze2 = addFirstLeft maze1 on
	let maze3 = addFirstLeft maze2 ok
	let maze4 = addFirstLeft maze3 on
	let maze41 = addFirstLeft maze4 on
	let maze42 = addFirstLeft maze41 hl
	let maze5 = addInRight maze42 hl
	let maze6 = addInRightLeft maze5 b
	let maze7 = addFirstLeft maze6 f
	let maze8 = addFirstLeft maze7 on
	let maze81 = addFirstLeft maze8 od
	let maze9 = addFirstLeft maze81 hl
	let maze10 = addFirstLeft maze9 s
	let maze11 = addFirstLeft maze10 b
	let maze12 = addFirstLeft maze11 b
	let maze13 = addFirstLeft maze12 on
	addFirstLeft maze13 MazeEnd