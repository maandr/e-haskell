module DatatypesSpec where

import Test.Hspec

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
        it "should run over all given animals" $ do
            -- given
            let animals = givenAnimals

            -- when
            let overrunAnimals = runOverAnimalsPure animals

            -- then
            overrunAnimals `shouldBe` [(Dillo False 30.3), (Rattlesnake 55.3 0.0), (Dillo False 45.5)]

        it "should run over empty list of animals" $ do
            runOverAnimalsPure [] `shouldBe` []

    describe "runOverAnimalsFMap" $ do
        it "should run over all given animals" $ do
            -- given
            let animals = givenAnimals

            -- when
            let overrunAnimals = runOverAnimalsFMap animals

            -- then
            overrunAnimals `shouldBe` [(Dillo False 30.3), (Rattlesnake 55.3 0.0), (Dillo False 45.5)]

givenAnimals :: [Animal]
givenAnimals =
    [ (Dillo True 30.3)
    , (Rattlesnake 55.3 4.0)
    , (Dillo True 45.5)
    ]