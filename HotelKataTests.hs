module HotelKataTests where

import HotelKata

type Expected = Maybe (BuildingNum, FloorNum, RoomNum)

main :: IO ()
main = putStrLn $ testsPassed runTests

testsPassed :: Bool -> String
testsPassed True = "All tests passed"
testsPassed False = "#NotAllTests" 

runTests :: Bool
runTests = all (\(expected,actual) -> expected == actual) $ map (fmap getOccupiedRoom) testHotels

testHotels :: [(Expected, String)]
testHotels = [failure, singleRoom, thirdBuilding, a, b, c, d, e]

failure = (Nothing, "#####")

singleRoom = ( Just (1,1,1), "*" )

thirdBuilding = ( Just (3,1,1), "#  #  *  #  #" )

a = ( Just (1,3,5),
     "#####\n" ++
     "#####\n" ++
     "####*\n" ++
     "#####\n" ++
     "#####" )

b = ( Just (9,1,1),
      "         #####\n" ++
      "         #####           ######\n" ++
      "         #####           ######\n" ++
      "#  #  #  #####  #  #  #  ######  *" )

c = ( Just (5,2,3),
      "#\n" ++
      "#  #\n" ++
      "#  #  ##\n" ++
      "#  #  ##  ###\n" ++
      "#  #  ##  ###  #####\n" ++
      "#  #  ##  ###  ##*##  ########\n" ++
      "#  #  ##  ###  #####  ########" )

d = ( Just (2,8,1),
    "          #\n" ++
    "          *\n" ++
    "          #\n" ++
    "          #\n" ++
    "          #\n" ++
    "          #\n" ++
    "          #\n" ++
    "          #\n" ++
    "########  #  #" )

e = ( Just (9,9,9),
    "                        ########*\n" ++
    "                        #########\n" ++
    "                        #########\n" ++
    "                        #########\n" ++
    "                        #########\n" ++
    "                        #########\n" ++
    "                        #########\n" ++
    "                        #########\n" ++
    "#  #  #  #  #  #  #  #  #########" )
