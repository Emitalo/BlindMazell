import Maze(Key, Door, Object (NoObject, ObjectDoor, ObjectKey, MazeEnd, Hole, Bear, Sword, Flashlight), Maze (NoExit), Player (Winner, Loser),
	createKey, createDoor, addFirstLeft, createPlayer, walkLeft, walkRight, walkBack, printMaze, addInRight, addInRightLeft, 
	playerHasAFlashlight, showNextSteps, showObjectInNextSteps, isSword, isKey, isDoor, isEnd, isBear, isHole, isFlashlight, deleteFlashlight)

startPlay :: IO ()
startPlay = do
	let maze = createScenario
	print "Ola jogador! Bem-vindo ao Mazell"
	print "Insira o seu nome"
	name <- getLine
	let player = createPlayer name maze
	play player 
	printMaze maze

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

data Option = OptionLeft {} | OptionRight {} | OptionBack {} | InvalidOption {} | TurnOnFlashlight

createOption :: String -> Option
createOption option 
	| option == "a" || option == "A" = OptionLeft
	| option == "d" || option == "D" = OptionRight
	| option == "f" || option == "F" = TurnOnFlashlight
	| option == "s" || option == "S" = OptionBack
	| otherwise = InvalidOption

getFlashlight :: Player -> Option
getFlashlight player 
	| playerHasAFlashlight player = TurnOnFlashlight 
	| not (playerHasAFlashlight player) = InvalidOption 

showPossibleWays :: Player -> String
showPossibleWays player 
	| (fst possibleWays) && (snd possibleWays) = "Voce pode ir para a direita ou para a esquerda"
	| (fst possibleWays) && (not (snd possibleWays)) = "Voce so pode ir para a esquerda"
	| (not (fst possibleWays)) && (snd possibleWays) = "Voce so pode ir para a direita"
	| (not (fst possibleWays)) && (not (snd possibleWays)) = "Voce nao pode ir nem para a esquerda nem para a direita"
	where 
		possibleWays = showNextSteps player

showObjectInLeftWithFlashLight :: Player -> String
showObjectInLeftWithFlashLight player
	| isSword (fst objects) = "A esquerda tem uma espada."
	| isFlashlight (fst objects) = "A esquerda tem uma lanterna."
	| isBear (fst objects) = "A esquerda tem um urso."
	| isHole (fst objects) = "A esquerda tem um buraco."
	| isKey  (fst objects) = "A esquerda tem uma chave."
	| isDoor (fst objects) = "A esquerda tem uma porta."
	| (fst objects) == NoObject = "A esquerda não tem nada."
	| isEnd (fst objects) = "A esquerda tem a saída."
	| otherwise = ""
	where
		objects = showObjectInNextSteps player

showObjectInRightWithFlashLight :: Player -> String
showObjectInRightWithFlashLight player
	| isSword (snd objects) = "A direita tem uma espada."
	| isFlashlight (snd objects) = "A direita tem uma lanterna."
	| isBear (snd objects) = "A direita tem um urso."
	| isHole (snd objects) = "A direita tem um buraco."
	| isKey  (snd objects) = "A direita tem uma chave."
	| isDoor (snd objects) = "A direita tem uma porta."
	| (snd objects) == NoObject = "A direita não tem nada."
	| isEnd (snd objects) = "A direita tem a saída."
	| otherwise = ""
	where
		objects = showObjectInNextSteps player


play :: Player -> IO()
play Winner = putStrLn "Este e o labirinto que voce estava jogando"
play Loser = putStrLn ""
play player = do
	putStrLn "\nComandos:\n"
	putStrLn "a - andar para a esquerda"
	putStrLn "d - andar para a direita"
	putStrLn "s - voltar"
	let opl = getFlashlight player
	case opl of
		TurnOnFlashlight -> putStrLn "f - ligar a lanterna"
		otherwise -> putStrLn ""
	let ways = showPossibleWays player
	putStrLn ways
	putStrLn "Pra onde deseja ir?"
	option <- getLine
	let op = createOption option
	putStrLn "\ESC[2J"
	case op of
		OptionLeft {}-> putStrLn walkLeftMessage >> play walkLeftPlayer
		OptionRight {}-> putStrLn walkRightMessage >> play walkRightPlayer	
		OptionBack {}-> putStrLn walkBackMessage >> play walkBackPlayer
		TurnOnFlashlight ->  putStrLn ("\n" ++ objleft) >> putStrLn ("\n" ++ objright) >> play curPlayer
		otherwise -> putStrLn "\nOpcao invalida" >> play player
		where
			objleft = showObjectInLeftWithFlashLight player
			objright = showObjectInRightWithFlashLight player
			curPlayer = deleteFlashlight player
			walkLeftMessage = "\n" ++ snd (walkLeft player)
			walkLeftPlayer = fst (walkLeft player)
			walkRightMessage = "\n" ++ snd (walkRight player)
			walkRightPlayer = fst (walkRight player)
			walkBackMessage = "\n" ++ snd (walkBack player)
			walkBackPlayer = fst (walkBack player)

