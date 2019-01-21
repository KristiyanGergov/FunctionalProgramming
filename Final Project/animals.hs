module Animals where

data BloodType = Cold_Blooded | Warm_Blooded   
           deriving (Eq, Show, Read, Enum)  

data AnimalType = Fish | Invertebrate | Amphibian | Reptile | Bird | Mammal           
           deriving (Eq, Show, Read, Enum)  

allBloodTypes = [Cold_Blooded .. ]
allAnimalTypes = [Fish .. ]

data Animal = Animal {
  name :: String,
  bloodType :: BloodType,
  animalType :: AnimalType,
  questions :: [String]
} deriving (Eq, Show, Read)

animals = [
  Animal { name = "Cat", bloodType = Warm_Blooded, animalType = Mammal, questions = []},
  Animal { name = "Dog", bloodType = Warm_Blooded, animalType = Mammal, questions = []},
  Animal { name = "Pig", bloodType = Warm_Blooded, animalType = Mammal, questions = []},
  Animal { name = "Pigeon", bloodType = Warm_Blooded, animalType = Bird, questions = []},
  Animal { name = "Lizard", bloodType = Cold_Blooded, animalType = Reptile , questions = []},
  Animal { name = "Snake", bloodType = Cold_Blooded, animalType = Reptile, questions = []},
  Animal { name = "Frog", bloodType = Cold_Blooded, animalType = Amphibian, questions = []},
  Animal { name = "Snail", bloodType = Cold_Blooded, animalType = Invertebrate, questions = []},
  Animal { name = "Swordfish", bloodType = Cold_Blooded, animalType = Fish, questions = []},
  Animal { name = "Dolphin", bloodType = Warm_Blooded, animalType = Mammal, questions = []},
  Animal { name = "Opah", bloodType = Warm_Blooded, animalType = Fish, questions = []},
  Animal { name = "Eagle", bloodType = Warm_Blooded, animalType = Bird, questions = []},
  Animal { name = "Elephant", bloodType = Warm_Blooded, animalType = Mammal, questions = []},
  Animal { name = "Bear", bloodType = Warm_Blooded, animalType = Mammal, questions = []},
  Animal { name = "Rhino", bloodType = Warm_Blooded, animalType = Mammal, questions = []}
  ]