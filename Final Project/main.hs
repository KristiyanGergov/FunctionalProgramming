data BloodType = Cold_Blooded | Warm_Blooded   
           deriving (Eq, Show, Read, Enum)  

data AnimalType = Air | Sea | Earth | AirSea | AirEarth | SeaEarth | Mixed
            deriving (Eq, Show, Read, Enum)
            
data Mammal = Yes | No
            deriving (Eq, Show, Read, Enum)                

allBloodTypes = [Cold_Blooded ..]
allAnimalTypes = [Air ..]
allMammals = [Yes ..]
                                 

data Animal = Animal {
  name :: String,
  bloodType :: BloodType,
  animalType :: AnimalType,
  mammal :: Mammal
} deriving (Eq, Show, Read)

animals = [
  Animal { name = "Cat", bloodType = Warm_Blooded, animalType = Earth, mammal = Yes},
  Animal { name = "Dog", bloodType = Warm_Blooded, animalType = Earth, mammal = Yes},
  Animal { name = "Pig", bloodType = Warm_Blooded, animalType = Earth, mammal = Yes},
  Animal { name = "Pigeon", bloodType = Warm_Blooded, animalType = Air, mammal = No},
  Animal { name = "Lizard", bloodType = Cold_Blooded, animalType = Mixed, mammal = No},
  Animal { name = "Snake", bloodType = Cold_Blooded, animalType = Earth, mammal = No},
  Animal { name = "Frog", bloodType = Cold_Blooded, animalType = SeaEarth, mammal = No},
  Animal { name = "Spider", bloodType = Cold_Blooded, animalType = Earth, mammal = No},
  Animal { name = "Scorpion", bloodType = Cold_Blooded, animalType = Earth, mammal = No},
  Animal { name = "Fish", bloodType = Cold_Blooded, animalType = Sea, mammal = No},
  Animal { name = "Elephant", bloodType = Warm_Blooded, animalType = Earth, mammal = Yes},
  Animal { name = "Bear", bloodType = Warm_Blooded, animalType = SeaEarth, mammal = Yes},
  Animal { name = "Rhino", bloodType = Warm_Blooded, animalType = Earth, mammal = Yes}
  ]

  
checkType _ [] _ _ = do 
  putStrLn "I give up. What type is it?"
  return []

checkType currentAnimals currentTypes criteria "Yes" = do 
  return (filter (\animal -> criteria animal == (head currentTypes)) currentAnimals)

checkType currentAnimals currentTypes criteria "No" = do
  let headAnimalType = (head currentTypes)
  let question = "Is it " ++ (show headAnimalType) ++ " animal? (Yes/No)"
  putStrLn question
  do
    answer <- getLine
    if answer == "Yes"
      then do
        checkType currentAnimals currentTypes criteria answer      
    else
      checkType currentAnimals (tail currentTypes) criteria answer      
      
main = do 
  filterByBloodType <- checkType animals allBloodTypes bloodType "No"
  
  filterByAnimalType <- checkType animals allAnimalTypes animalType "No"

  putStrLn (show filterByAnimalType)