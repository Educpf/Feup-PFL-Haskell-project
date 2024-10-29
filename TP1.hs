import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type AdjList = [(City,[(City,Distance)])]
type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)
type DijkstraList = Data.Array.Array Int (Bool, Distance, [Int])
type MemoizationTable = Data.Array.Array (Int, Int) (Maybe Distance)
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


-- DIJKSTRA

getDijkstraPath :: DijkstraList -> Int -> [[City]] -- Use cities as Int
getDijkstraPath list dest  | null back = [[show dest]]
                            | otherwise =  map ( ++ [show dest] ) (foldr (\elem acc -> acc ++ getDijkstraPath list elem) [] back)
                                where (visited,_,back) = list Data.Array.! dest

getSmallerUnvisited :: DijkstraList -> Int
getSmallerUnvisited list = result
                where (_, result, _) = foldl (\(i, bi, bd) (v, d, _)  -> if not v && (d < bd) then (i+1, i, d) else (i+1, bi, bd)) (0, -1, maxBound) (Data.Array.elems list)

updateConnection :: AdjMatrix -> DijkstraList -> Int -> Int -> (Bool, Distance, [Int]) -- Only one element
updateConnection matrix list origin dest | origin == dest = (True, d_o, p_o)
                                         | distance == Nothing = (v_d,d_d,p_d)
                                         | newDistance <  d_d = (v_d, newDistance, [origin])
                                         | newDistance ==  d_d = (v_d, d_d, origin : p_d)
                                         | otherwise = (v_d, d_d, p_d)
                                            where
                                                (v_o,d_o,p_o) = list Data.Array.! origin
                                                (v_d,d_d,p_d) = list Data.Array.! dest
                                                distance = matrix Data.Array.! (origin, dest)
                                                newDistance = case distance of
                                                                Nothing -> d_o
                                                                Just num -> num + d_o

updateConnections :: AdjMatrix -> DijkstraList -> Int -> DijkstraList
updateConnections con list node = Data.Array.array (0, maxI) [ (i, updateConnection con list node i) | i <- [0..maxI]]
                            where (_, maxI)= Data.Array.bounds list

dijkstra :: AdjMatrix -> DijkstraList -> Int -> Int -> [Path]
dijkstra con list o d | nextNode == -1 = getDijkstraPath list d
                      | otherwise = dijkstra con updatedList o d-- Usar o getsmallerUnvisited já pra me dar se todos foram visitados ou não!! Se retornar -1 então são todos visitados!!!
                            where
                                updatedList = updateConnections con list nextNode
                                nextNode = getSmallerUnvisited list

createAdjMatrix :: RoadMap -> AdjMatrix
createAdjMatrix rm = Data.Array.array ((0,0), (ncities,ncities)) [((x,y),distance rm (show x) (show y))|  x <- [0..ncities], y <- [0..ncities]]
                    where ncities = length (cities rm) -1

createDijkstraTable :: Int -> Int -> DijkstraList
createDijkstraTable len origin = Data.Array.array (0,len) [if i == origin then (i,(False, 0, [])) else(i,(False, maxBound, [])) | i <- [0..len]]

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm origin destiny = dijkstra adjMatrix (createDijkstraTable numCities (read origin)) (read origin) (read destiny)
                                    where adjMatrix = createAdjMatrix rm
                                          (_, numCities) = snd (Data.Array.bounds adjMatrix)

-- 

fillTable :: AdjMatrix -> MemoizationTable -> Int -> Int -> MemoizationTable
fillTable matrix table visited origin | storedValue /= Nothing = table -- Value already stored in table
                                      | 1 Data.Bits.shiftL numCities - 1 == updatedVisited = table Data.Array.// [((visited, origin), distanceToZero)] -- All nodes already visited
                                      | otherwise = foldl (\acc elem -> fillTable matrix acc updatedVisited elem) table [i | i <- [0..numCities - 1], (1 Data.Bits.shiftL i Data.Bits..&. updatedVisited) == 0, (matrix Data.Array.! (origin, i)) /= Nothing]
                                        where
                                            (_, numCities) = snd (Data.Array.bounds matrix)
                                            storedValue = table Data.Array.! (visited, origin)
                                            updatedVisited = visited Data.Bits..|. 1 Data.Bits.shiftL origin
                                            distanceToZero = matrix Data.Array.! (origin, 0)

createMemoizationTable :: Int -> MemoizationTable
createMemoizationTable size = Data.Array.array ((0, 0), (size ^ 2, size)) [ ((x, y), Nothing) | x <- [0..size^2], y <- [0 .. size]]

createPath :: MemoizationTable -> Int -> Path
createPath memo visited = 
                        where  
                            column = [memo Data.Array.! (visited, i) | i <- [0..]]
                            (_, numCities) = snd (memo Data.Array.bounds memo)

travelSales :: RoadMap -> Path
travelSales rm = getPath (fillTable adjMAtrix (createMemoizationTable numCities) 0 0)
                where
                    adjMatrix = createAdjMatrix rm
                    (_, numCities) = snd (Data.Array.bounds adjMatrix)

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap
gTest4 = [("0","1",10),("0","2",15),("1","2",35),("1","3",25),("2","3",20)]