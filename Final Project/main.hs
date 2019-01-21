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

checkAnimal currentAnimals = do
    let current = (head currentAnimals)
    let question = "Is it " ++ (name current) ++ "? (Yes/No)"
    putStrLn question 
    answer <- getLine
    
    if answer == "Yes"
        then do
            putStrLn "Great!"
            return [(head currentAnimals)]
    else if (length currentAnimals) == 1
        then do
          putStrLn "I give up. What is it?"
          answer <- getLine
          
          let questionToDistinguish = "How do I distinguish " ++ answer ++ " from " ++ (name current) ++ "?"

          putStrLn questionToDistinguish
          newQuestion <- getLine
          
          let newAnimal = Animal { name = answer, bloodType = (bloodType current), animalType = (animalType current), questions = [newQuestion] }
          return (animals ++ [newAnimal])
    else    
        checkAnimal (tail currentAnimals)       


checkQuestions [] = do
  putStr ""        
  return []
checkQuestions currentAnimals = do
  let currentAnimalQuestions = questions (head currentAnimals) 
  if currentAnimalQuestions /= []
    then do
      putStrLn (head currentAnimalQuestions)
      answer <- getLine
      
      if answer == "Yes"
       then do
        return (filter (\animal -> questions animal == currentAnimalQuestions) currentAnimals) 
       else
        checkQuestions (tail currentAnimals) 
  else
    checkQuestions (tail currentAnimals)

oneMore answer = do
  putStrLn "One more? (Yes/No)"

  oneMore <- getLine

  if oneMore == "Yes"
    then do
      if (tail answer == [])
        then do
          start animals
      else
        start answer
  else
    putStrLn "Bye!"   
    
start :: [Animal] -> IO()
start animals = do 
  animalsFilteredByBloodType <- checkType animals allBloodTypes bloodType "No"
  
  animalsFilteredByAnimalType <- checkType animalsFilteredByBloodType allAnimalTypes animalType "No"

  animalsFilteredByQuestions <- checkQuestions animalsFilteredByAnimalType

  if animalsFilteredByQuestions == []
    then do
      answer <- checkAnimal animalsFilteredByAnimalType
      oneMore answer
    else 
      do
        answer <- checkAnimal animalsFilteredByQuestions 
        oneMore answer

main = start animals