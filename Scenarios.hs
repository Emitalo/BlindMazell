module Scenarios(scenario1, scenario2) where

import Maze(Key, Door, Object (NoObject, ObjectDoor, ObjectKey, MazeEnd, Hole, Bear, Sword, Flashlight), Maze (NoExit), Player (Winner, Loser),
	addFirstLeft, addFirstRight, createPlayer, walkLeft, walkRight, walkBack, printMaze, addInRight, addInRightLeft, 
	playerHasAFlashlight, showNextSteps, showObjectInNextSteps, isSword, isKey, isDoor, isEnd, isBear, isHole, isFlashlight, deleteFlashlight)

import Objects(Key, Door(doorKey), Object (NoObject, ObjectDoor, objectDoor, ObjectKey, objectKey, MazeEnd, Hole, Bear, Sword, Flashlight),  
	createKey, createDoor, toString, isSword, isKey, isDoor, isEnd, isBear, isHole, isFlashlight)

scenario1 :: Maze
scenario1 = do
	
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

scenario2 :: Maze
scenario2 = do
	let k = createKey 3
	let d = createDoor k

	let ok = ObjectKey k
	let od = ObjectDoor d
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
	let maze5 = addFirstLeft maze4 on
	let maze6 = addFirstLeft maze5 hl

	let maze7 = addFirstLeft maze6 f
	let maze8 = addFirstLeft maze7 on
	
	let maze9 = addFirstLeft maze8 od
	let maze10 = addFirstLeft maze9 hl

	let maze11 = addFirstLeft maze10 s
	let maze12 = addFirstLeft maze11 b

	let maze13 = addFirstLeft maze12 b
	let maze14 = addFirstLeft maze13 on

	let maze15 = addFirstRight maze14 b
	let maze16 = addFirstRight maze15 hl
	
	let maze17 = addFirstRight maze16 on
	let maze18 = addFirstRight maze17 MazeEnd

	let maze19 = addFirstRight maze18 hl
	addFirstRight maze19 on