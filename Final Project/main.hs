data BloodType = Cold_Blooded | Warm_Blooded   
           deriving (Eq, Show, Read, Enum)  

data AnimalType = Fish | Invertebrate | Amphibian | Reptile | Bird | Mammal           
           deriving (Eq, Show, Read, Enum)  

allBloodTypes = [Cold_Blooded .. ]
allAnimalTypes = [Fish .. ]
specialQuestions = []

data Animal = Animal {
  name :: String,
  bloodType :: BloodType,
  animalType :: AnimalType,
  questions :: [String]
} deriving (Eq, Show, Read)

animals = [
  Animal { name = "Cat", bloodType = Warm_Blooded, animalType = Mammal      , questions = []},
  Animal { name = "Dog", bloodType = Warm_Blooded, animalType = Mammal      , questions = []},
  Animal { name = "Pig", bloodType = Warm_Blooded, animalType = Mammal      , questions = []},
  Animal { name = "Pigeon", bloodType = Warm_Blooded, animalType = Bird     , questions = []},
  Animal { name = "Lizard", bloodType = Cold_Blooded, animalType = Reptile  , questions = []},
  Animal { name = "Snake", bloodType = Cold_Blooded, animalType = Reptile   , questions = []},
  Animal { name = "Frog", bloodType = Cold_Blooded, animalType = Amphibian  , questions = []},
  Animal { name = "Swordfish", bloodType = Cold_Blooded, animalType = Fish  , questions = []},
  Animal { name = "Dolphin", bloodType = Warm_Blooded, animalType = Mammal  , questions = []},
  Animal { name = "Elephant", bloodType = Warm_Blooded, animalType = Mammal , questions = []},
  Animal { name = "Bear", bloodType = Warm_Blooded, animalType = Mammal     , questions = []},
  Animal { name = "Rhino", bloodType = Warm_Blooded, animalType = Mammal    , questions = []}
  ]

  
checkType _ [] _ _ = do 
  putStrLn "I give up. What type is it?"
  answer <- getLine
  return []

checkType currentAnimals currentTypes criteria "Yes" = do 
  return (filter (\animal -> criteria animal == (head currentTypes)) currentAnimals)

checkType currentAnimals currentTypes criteria "No" = do
  let headAnimalType = (head currentTypes)
  let question = "Is it " ++ (show headAnimalType) ++ "? (Yes/No)"
  putStrLn question
  do
    answer <- getLine
    if answer == "Yes"
      then do
        checkType currentAnimals currentTypes criteria answer      
    else
      checkType currentAnimals (tail currentTypes) criteria answer      

checkAnimal :: [Animal] -> IO ()
checkAnimal [] = do
     putStrLn "I give up. What is it?"
     answer <- getLine
     putStrLn answer

checkAnimal x = do
    let question = "Is it " ++ (name (head x)) ++ "?"
    putStrLn question 
    answer <- getLine
    
    if answer == "Yes"
        then do
            putStrLn "Great!"
    else
        checkAnimal (tail x)        

main = do 
  animalsFilteredByBloodType <- checkType animals allBloodTypes bloodType "No"
  
  animalsFilteredByAnimalType <- checkType animalsFilteredByBloodType allAnimalTypes animalType "No"

  answer <- checkAnimal animalsFilteredByAnimalType

  putStrLn (show answer)