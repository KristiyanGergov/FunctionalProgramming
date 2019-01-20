data BloodType = Cold_Blooded | Warm_Blooded   
           deriving (Eq, Show, Read, Enum)  

data AnimalType = Fish | Invertebrate | Amphibian | Reptile | Bird | Mammal           

allBloodTypes = [Cold_Blooded ..]
allAnimalTypes = [Fish ..]

data Animal = Animal {
  name :: String,
  bloodType :: BloodType,
  animalType :: AnimalType,
} deriving (Eq, Show, Read)

animals = [
  Animal { name = "Cat", bloodType = Warm_Blooded, animalType = Mammal },
  Animal { name = "Dog", bloodType = Warm_Blooded, animalType = Mammal },
  Animal { name = "Pig", bloodType = Warm_Blooded, animalType = Mammal },
  Animal { name = "Pigeon", bloodType = Warm_Blooded, animalType = Bird },
  Animal { name = "Lizard", bloodType = Cold_Blooded, animalType = Reptile },
  Animal { name = "Snake", bloodType = Cold_Blooded, animalType = Reptile },
  Animal { name = "Frog", bloodType = Cold_Blooded, animalType = Amphibian },
  Animal { name = "Scorpion", bloodType = Cold_Blooded, animalType = Earth },
  Animal { name = "Swordfish", bloodType = Cold_Blooded, animalType = Fish },
  Animal { name = "Dolphin", bloodType = Warm_Blooded, animalType = Mammal },
  Animal { name = "Elephant", bloodType = Warm_Blooded, animalType = Mammal },
  Animal { name = "Bear", bloodType = Warm_Blooded, animalType = Mammal },
  Animal { name = "Rhino", bloodType = Warm_Blooded, animalType = Mammal }
  ]

  
checkType _ [] _ _ = do 
  putStrLn "I give up. What type is it?"
  return []

checkType currentAnimals currentTypes criteria "Yes" = do 
  return (filter (\animal -> criteria animal == (head currentTypes)) currentAnimals)

checkType currentAnimals currentTypes criteria "No" = do
  let headAnimalType = (head currentTypes)
  let question = "Is it a/an" ++ (show headAnimalType) ++ " animal? (Yes/No)"
  putStrLn question
  do
    answer <- getLine
    if answer == "Yes"
      then do
        checkType currentAnimals currentTypes criteria answer      
    else
      checkType currentAnimals (tail currentTypes) criteria answer      
      
main = do 
  animalsFilteredByBloodType <- checkType animals allBloodTypes bloodType "No"
  
  animalsFilteredByAnimalType <- checkType animalsFilteredByBloodType allAnimalTypes animalType "No"

  animalsFilteredByMammalType <- checkType animalsFilteredByAnimalType allMammals mammal "No"

  let answer = "Is it " ++ (name (head animalsFilteredByMammalType)) ++ "?"
  putStrLn answer