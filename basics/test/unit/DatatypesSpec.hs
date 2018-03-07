module DatatypesSpec where

import Test.Hspec

-- typeclass
class ToString a where
    toString :: a -> String

-- implementation of typeclass ToString for type Mood
instance ToString Mood where
    toString mood = show mood

-- implementation of typeclass ToString for type Int
instance ToString Int where
    toString int = show int

data Mood = Good | Bad deriving ( Eq, Show )

changeMood :: Mood -> Mood
changeMood Good = Bad
changeMood Bad = Good

data Animal = 
    Dillo Bool Float --alive, weight
    | Rattlesnake Float Float -- length, width
    deriving ( Eq, Show )

runOverAnimal :: Animal -> Animal
runOverAnimal (Dillo _ weight) = Dillo False weight
runOverAnimal (Rattlesnake length _) = Rattlesnake length 0.0

runOverAnimalsPure :: [Animal] -> [Animal]
runOverAnimalsPure [] = []
runOverAnimalsPure (x:xs) = runOverAnimal x : runOverAnimalsPure xs

runOverAnimalsFMap :: [Animal] -> [Animal]
runOverAnimalsFMap animals = fmap runOverAnimal animals

spec :: Spec
spec = do

    describe "toString" $ do
        it "should convert a mood to a string" $ do
            -- given
            let myMood = Bad :: Mood

            -- when + then
            toString myMood `shouldBe` "Bad"
        it "should convert a integer to a string" $ do
            -- given
            let int = 99 :: Int

            -- when + then
            toString int `shouldBe` "99"

    describe "changeMood" $ do
        it "should inverse the current value of mood" $ do
            -- given
            let myMood = Bad :: Mood
            
            -- when + then
            changeMood myMood `shouldBe` Good

    describe "runOverAnimal" $ do
        it "should kill a dillo when overrunning it" $ do
            -- given
            let dillo = Dillo True 45.5

            -- when
            let overrunDillo = runOverAnimal dillo

            -- then
            overrunDillo `shouldBe` Dillo False 45.5

        it "should flat a rattlesnake when overrunning it" $ do
            -- given
            let snake = Rattlesnake 115.5 5.3

            -- when
            let overrunSnake = runOverAnimal snake

            -- then
            overrunSnake `shouldBe` Rattlesnake 115.5 0.0
    describe "runOverAnimalsPure" $ do
        it "should apply runOverAnimal to all animals in a given list" $ do
            -- given
            let animals = givenAnimals

            -- when
            let overrunAnimals = runOverAnimalsPure animals

            -- then
            overrunAnimals `shouldBe` [(Dillo False 30.3), (Rattlesnake 55.3 0.0), (Dillo False 45.5)]

        it "should return empty list when given a empty list" $ do
            runOverAnimalsPure [] `shouldBe` []

    describe "runOverAnimalsFMap" $ do
        it "should apply runOverAnimal to all animals in a given list" $ do
            -- given
            let animals = givenAnimals

            -- when
            let overrunAnimals = runOverAnimalsFMap animals

            -- then
            overrunAnimals `shouldBe` [(Dillo False 30.3), (Rattlesnake 55.3 0.0), (Dillo False 45.5)]
        it "should return empty list when given a empty list" $ do
            runOverAnimalsFMap [] `shouldBe` []

givenAnimals :: [Animal]
givenAnimals =
    [ (Dillo True 30.3)
    , (Rattlesnake 55.3 4.0)
    , (Dillo True 45.5)
    ]