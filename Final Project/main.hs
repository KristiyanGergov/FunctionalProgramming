import Animals

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

checkAnimal x = do
    let current = (head x)
    let question = "Is it " ++ (name current) ++ "?"
    putStrLn question 
    answer <- getLine
    
    if answer == "Yes"
        then do
            putStrLn "Great!"
            return (head x)
    else if (length x) == 1
        then do
          putStrLn "I give up. What is it?"
          answer <- getLine
          
          let questionToDistinguish = "How do I distinguish " ++ answer ++ " from " ++ (name current) ++ "?"
          putStrLn questionToDistinguish
          newQuestion <- getLine
          let newAnimal = Animal { name = answer, bloodType = (bloodType current), animalType = (animalType current), questions = [newQuestion] }
          return newAnimal
    else    
        checkAnimal (tail x)        

addAnimal x = do
  let gosho = read x
  animals ++ gosho

main = do 
  animalsFilteredByBloodType <- checkType animals allBloodTypes bloodType "No"
  
  animalsFilteredByAnimalType <- checkType animalsFilteredByBloodType allAnimalTypes animalType "No"

  answer <- checkAnimal animalsFilteredByAnimalType

  -- let test = Animal { name = answer, bloodType = (bloodType current), animalType = (animalType current), questions = [] }

  putStrLn (show (name answer))