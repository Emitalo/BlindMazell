module Objects(Key(key), Door(doorKey), Object (NoObject, ObjectDoor, objectDoor, ObjectKey, objectKey, MazeEnd, Hole, Bear, Sword, Flashlight),  
	createKey, createDoor, toString, isSword, isKey, isDoor, isEnd, isBear, isHole, isFlashlight) where

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
		NoObject {} -> "Nada aqui"
		ObjectKey {objectKey = ok} -> "Chave " ++ ( show (key ok))
		ObjectDoor {objectDoor = od} -> "Porta (chave: " ++ (show (key (doorKey od))) ++")"
		MazeEnd {} -> "Saida"
		Hole {} -> "Buraco"
		Bear {} -> "Urso"
		Flashlight {} -> "Lanterna"
		Sword {} -> "Espada"

