import Maze(Key, Door, Object (NoObject, ObjectDoor, ObjectKey, MazeEnd), Maze (NoExit), Player (Winner),
	createKey, createDoor, openDoor, addFirstLeft, createPlayer, walkLeft, walkRight, printMaze)

--startPlay :: Player
startPlay = do
	let maze = createScenario
	print "Ola jogador! Bem-vindo ao Mazell"
	print "Insira o seu nome"
	name <- getLine
	let player = createPlayer name maze
	play player 
	--print (player)

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

	-- Create mazes
	let maze = addFirstLeft NoExit on
	let maze1 = addFirstLeft maze ok
	let maze2 = addFirstLeft maze1 on
	let maze3 = addFirstLeft maze2 od
	let maze4 = addFirstLeft maze3 on
	let maze5 = addFirstLeft maze4 on
	let maze6 = addFirstLeft maze5 on
	let maze7 = addFirstLeft maze6 ok2
	let maze8 = addFirstLeft maze7 on
	let maze9 = addFirstLeft maze8 od2
	addFirstLeft maze9 MazeEnd

data Option = OptionLeft {}| OptionRight {}| OptionBack {}

createOption :: String -> Option
createOption option 
	| option == "a" || option == "A" = OptionLeft
	| option == "d" || option == "D" = OptionRight
	| option == "s" || option == "S" = OptionBack
	| otherwise = error "Opcao invalida"

play :: Player -> IO()
play Winner = putStrLn ""
play player = do
	putStrLn "\nComandos:\n"
	putStrLn "a - andar para a esquerda"
	putStrLn "d - andar para a direita"
	putStrLn "s - voltar"
	option <- getLine
	let op = createOption option
	case op of
		OptionLeft {}-> putStrLn (snd (walkLeft player)) >> play (fst (walkLeft player))
		OptionRight {}-> putStrLn (snd (walkRight player)) >> play (fst (walkRight player))	
		OptionBack {}-> putStrLn "Back"
		otherwise -> error "Opcao invalida"
