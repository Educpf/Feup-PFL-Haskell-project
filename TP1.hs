import qualified Data.List
import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type AdjList = [(City,[(City,Distance)])]
type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)
type DijkstraList = Data.Array.Array Int (Bool, Distance, [Int])
type RoadMap = [(City,City,Distance)]


convertToAdjList :: RoadMap -> AdjList
convertToAdjList rm = [(city, adjacent rm city) | city <- cities rm]


cities :: RoadMap -> [City]
cities = Data.List.nub . foldr (\ (c1,c2,_) acc -> c1 : c2 : acc) []

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = or [(c1==x && c2==y) || (c2==x && c1==y) | (x,y,_) <- rm]
-- areAdjacent rm c1 c2 = any (\(x,y,_) -> (c1==x && c2==y) || (c2==x && c1==y)) rm 

distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2 = fmap (\(_,_,d) -> d) (Data.List.find (\(x,y,_) -> (c1==x && c2==y) || (c2==x && c1==y)) rm)

adjacent :: RoadMap -> City -> [(City,Distance)]
-- adjacent rm c = [ (y,d)  | (x,y,d) <- rm, x == c] ++ [(x,d)| (x,y,d) <- rm, y == c]
adjacent rm c = [ if  x == c then (y,d) else (x, d) | (x,y,d) <- rm, x == c || y == c]

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance rm p = foldr (\ v acc -> case (v,acc) of
                                            (Nothing,_) -> Nothing
                                            (_,Nothing) -> Nothing
                                            (Just x,Just tot) -> Just (x + tot) ) (Just 0) [distance rm x y | (x,y) <- zip p (tail p)]


rome :: RoadMap -> [City]
--  rome rm =
--     let degrees = [(x,Data.List.length [c | c <- foldr (\ (c1,c2,_) acc -> c1 : c2 : acc) [] rm, c == x ]) | x <- cities rm]
--         max = Data.List.maximum (map snd degrees)
--     in [c | (c,d) <- degrees, d == max]
rome rm = [ c | (c, d) <- cityDegree, d == maxDegree ]
        where
            cityDegree = map (\(c, l)-> (c, Data.List.length l)) (convertToAdjList rm)
            maxDegree = Data.List.maximum (map snd cityDegree)


-- array ! 2

dfs :: AdjList -> City -> [City] -> [City]
dfs al c visited    |  c `elem` visited = visited
                    | otherwise = foldr (dfs al) (c : visited) children
        where
            children =  head [ map fst d | (city, d) <- al, city == c]


isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = length (dfs (convertToAdjList rm) (head (cities rm)) []) == totalCity
                        where totalCity = length (cities rm)


getDijkstraPath :: DijkstraList -> Int -> [[City]] -- Use cities as Int
getDijkstraPath list dest  = map (show dest : ) (foldr (\elem acc -> acc ++ getDijkstraPath list elem) [] back)
                                where (visited,_,back) = list Data.Array.! dest


dijkstra :: AdjMatrix -> DijkstraList -> City -> City -> [Path]
dijkstra con list o d | allvisited list = getDijkstraPath list d
                      | otherwise = dijkstra con updatedList o d
                            where updatedList = updateConnections con (getSmallerUnvisited list) list


shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]