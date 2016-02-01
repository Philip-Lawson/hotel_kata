import Data.String

type Hotel = [String]
type BuildingNum = Int
type FloorNum = Int
type RoomNum = Int
type FloorPlan = String
type OccupiedFloor = String

parseHotel :: String -> Maybe (FloorPlan, Hotel)
parseHotel [] = Nothing
parseHotel input = return (x, x:xs)
             where (x:xs) = reverse $ lines input

whichFloor :: Hotel -> Maybe (FloorNum, OccupiedFloor)
whichFloor [] = Nothing
whichFloor list = go list 1
              where go [] _ = Nothing
                    go (x:xs) n | '*' `elem` x = return (n, x)
                                | otherwise  = go xs (n+1)

getRoomNum :: FloorPlan -> OccupiedFloor -> Maybe (BuildingNum, RoomNum) 
getRoomNum floorPlan occupiedFloor = return (length buildings, length (last buildings))
                        where buildings   = words $ take cutoffPoint floorPlan 
                              cutoffPoint = length $ dropWhile (/= '*') $ reverse occupiedFloor 

getRoom :: String -> Maybe (BuildingNum, FloorNum, RoomNum)
getRoom input = do
      (floorPlan, hotel) <- parseHotel input
      (floorNum,  occupiedFloor) <- whichFloor hotel
      (buildingNum, roomNum) <- getRoomNum floorPlan occupiedFloor
      return (buildingNum, floorNum, roomNum)
