import Maze(Key, Door, Object (NoObject, ObjectDoor, ObjectKey, MazeEnd, Hole, Bear, Sword, Flashlight), Maze (NoExit), Player (Winner, Loser),
	createKey, createDoor, openDoor, addFirstLeft, createPlayer, walkLeft, walkRight, printMaze, addInRight)

--startPlay :: Player
startPlay = do
	let maze = createScenario
	print "Ola jogador! Bem-vindo ao Mazell"
	print "Insira o seu nome"
	name <- getLine
	let player = createPlayer name maze
	printMaze maze
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

	let hl = Hole
	let b = Bear
	let f = Flashlight
	let s = Sword

	-- Create mazes
	let maze = addFirstLeft NoExit on
	let maze1 = addFirstLeft maze s
	let maze2 = addFirstLeft maze1 ok
	let maze3 = addFirstLeft maze2 b
	let maze4 = addFirstLeft maze3 s
	let maze5 = addFirstLeft maze4 f
	let maze6 = addFirstLeft maze5 b
	let maze7 = addFirstLeft maze6 ok2
	let maze8 = addFirstLeft maze7 on
	let maze9 = addFirstLeft maze8 od2
	let maze10 = addFirstLeft maze9 hl
	addFirstLeft maze10 MazeEnd

data Option = OptionLeft | OptionRight 

createOption :: String -> Option
createOption option 
	| option == "a" || option == "A" = OptionLeft
	| option == "d" || option == "D" = OptionRight
	| otherwise = error "Opcao invalida"

play :: Player -> IO()
play Winner = putStrLn ""
play Loser = putStrLn ""
play player = do
	putStrLn "\nComandos:\n"
	putStrLn "a - andar para a esquerda"
	putStrLn "d - andar para a direita"
	putStrLn "Pra onde deseja ir?"
	option <- getLine
	let op = createOption option
	case op of
		OptionLeft -> putStrLn (snd (walkLeft player)) >> play (fst (walkLeft player))
		OptionRight -> putStrLn (snd (walkRight player)) >> play (fst (walkRight player))	
		otherwise -> error "Opcao invalida"
