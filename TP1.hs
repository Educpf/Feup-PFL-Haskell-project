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

getNumCities :: AdjMatrix -> Int
getNumCities matrix = snd (snd (Data.Array.bounds matrix)) + 1


fillTable :: AdjMatrix -> MemoizationTable -> Int -> Int -> MemoizationTable
fillTable matrix table visited origin | storedValue /= Nothing = table -- Value already stored in table, return table
                                        -- all visited besides current
                                      | Data.Bits.shiftL 1 numCities - 1 == updatedVisited = table Data.Array.// [((visited, origin), distanceToZero)] -- All nodes already visited, complete table
                                      | otherwise = updatedTable Data.Array.// [((visited, origin), Just distance)]-- Get table from children and add entry of 
                                        where
                                            numCities = getNumCities matrix
                                            -- Get value stored in memoizationTable for current position
                                            storedValue = table Data.Array.! (visited, origin)
                                            -- Make current node visited
                                            updatedVisited = visited Data.Bits..|. Data.Bits.shiftL 1 origin
                                            distanceToZero = matrix Data.Array.! (origin, 0)
                                            -- Get unvisited nodes connected to current node
                                            nextNodes = [i | i <- [0..numCities - 1], (Data.Bits.shiftL 1 i Data.Bits..&. updatedVisited) == 0, (matrix Data.Array.! (origin, i)) /= Nothing]
                                            -- Get memoizationTable updated by descendants and the best next city to follow 
                                            (updatedTable, distance, nextCity) = foldl (\(accTable, bestDistance, bestI) elem ->  -- Recursive update of tables and find best Distance
                                                                                                        let
                                                                                                            updatedTable = fillTable matrix accTable updatedVisited elem
                                                                                                            distanceFromNode = updatedTable Data.Array.! (updatedVisited, elem)
                                                                                                            distanceToNode = matrix Data.Array.! (origin, elem)
                                                                                                            totalDistance = case (distanceFromNode, distanceToNode) of 
                                                                                                                        (Nothing, _) -> maxBound
                                                                                                                        (_, Nothing) -> maxBound
                                                                                                                        (Just d1, Just d2) -> if distanceFromNode == Just maxBound then maxBound else d1 + d2
                                                                                                            in if totalDistance < bestDistance then (updatedTable,totalDistance,elem) else (updatedTable, bestDistance,bestI)) (table, maxBound, -1) nextNodes

createMemoizationTable :: Int -> MemoizationTable
createMemoizationTable size = Data.Array.array ((0, 0), (maxRow, maxColumn)) [ ((x, y), Nothing) | x <- [0..maxRow], y <- [0 .. maxColumn]]
                                where 
                                    maxColumn = size - 1 
                                    maxRow = (2 ^ size) -1

createPath :: AdjMatrix -> MemoizationTable -> Int -> Int -> Path -- update create path
createPath matrix memoTable visited lastCity    |  Data.Bits.shiftL 1 numCities - 1 == visited = ["0"] -- all visited Return 0
                                            | nextCity == -1 = []
                                            | otherwise = show lastCity : createPath matrix memoTable updatedVisited nextCity
                                                where  

                                                    nextNodes = [memoTable Data.Array.! (visited, i) | i <- [0..numCities - 1], (Data.Bits.shiftL 1 i Data.Bits..&. updatedVisited) == 0, (matrix Data.Array.! (lastCity, i)) /= Nothing]
                                                    numCities = snd (snd (Data.Array.bounds memoTable)) + 1
                                                    updatedVisited = visited Data.Bits..|. Data.Bits.shiftL 1 nextCity
                                                    (i, nextCity, distance) = foldl (\(i, bi, bd) elem -> 
                                                                    let 
                                                                        distanceToNode = matrix Data.Array.! (lastCity, i)
                                                                        totalDistance = case (distanceToNode, elem) of
                                                                                    (_, Nothing) -> maxBound
                                                                                    (Nothing, _) -> maxBound
                                                                                    (Just v1, Just v2) -> v1 + v2
                                                                        in if totalDistance < bd then (i+1, i, totalDistance) else (i+1,bi,bd)) (0, -1 , maxBound) nextNodes


travelSales :: RoadMap -> Path
travelSales rm = createPath adjMatrix (fillTable adjMatrix (createMemoizationTable (numCities+1) ) 0 0) 1 0
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

gTest5 :: RoadMap
gTest5 = [("0","1",4),("0","2",1),("2","3",1), ("3","4",1), ("4", "1", 1), ("0","5",2),("5","1",2)]

gTest6 :: RoadMap
gTest6 = [("0", "1", 6), ("0", "2", 6), ("0", "3", 46), ("0", "4", 31), ("0", "5", 51), ("0", "6", 70), ("0", "7", 18), ("0", "8", 29), ("0", "9", 63), ("0", "10", 18), ("0", "11", 78), ("0", "12", 69), ("0", "13", 43), ("0", "14", 36), ("0", "15", 6), ("0", "16", 51), ("0", "17", 0), ("0", "18", 65), ("0", "19", 86), ("1", "2", 25), ("1", "3", 87), ("1", "4", 99), ("1", "5", 40), ("1", "6", 82), ("1", "7", 61), ("1", "8", 29), ("1", "9", 18), ("1", "10", 91), ("1", "11", 58), ("1", "12", 31), ("1", "13", 91), ("1", "14", 56), ("1", "15", 45), ("1", "16", 42), ("1", "17", 27), ("1", "18", 91), ("1", "19", 6), ("2", "3", 15), ("2", "4", 15), ("2", "5", 43), ("2", "6", 63), ("2", "7", 29), ("2", "8", 18), ("2", "9", 63), ("2", "10", 77), ("2", "11", 35), ("2", "12", 90), ("2", "13", 40), ("2", "14", 71), ("2", "15", 45), ("2", "16", 65), ("2", "17", 82), ("2", "18", 39), ("2", "19", 19), ("3", "4", 69), ("3", "5", 72), ("3", "6", 49), ("3", "7", 89), ("3", "8", 6), ("3", "9", 72), ("3", "10", 45), ("3", "11", 52), ("3", "12", 46), ("3", "13", 7), ("3", "14", 80), ("3", "15", 99), ("3", "16", 98), ("3", "17", 92), ("3", "18", 77), ("3", "19", 2), ("4", "5", 3), ("4", "6", 24), ("4", "7", 57), ("4", "8", 95), ("4", "9", 59), ("4", "10", 41), ("4", "11", 80), ("4", "12", 0), ("4", "13", 5), ("4", "14", 25), ("4", "15", 48), ("4", "16", 13), ("4", "17", 57), ("4", "18", 61), ("4", "19", 99), ("5", "6", 21), ("5", "7", 75), ("5", "8", 33), ("5", "9", 83), ("5", "10", 92), ("5", "11", 59), ("5", "12", 80), ("5", "13", 25), ("5", "14", 8), ("5", "15", 73), ("5", "16", 19), ("5", "17", 9), ("5", "18", 84), ("5", "19", 68), ("6", "7", 91), ("6", "8", 10), ("6", "9", 27), ("6", "10", 92), ("6", "11", 49), ("6", "12", 54), ("6", "13", 84), ("6", "14", 79), ("6", "15", 5), ("6", "16", 14), ("6", "17", 87), ("6", "18", 66), ("6", "19", 53), ("7", "8", 47), ("7", "9", 1), ("7", "10", 78), ("7", "11", 53), ("7", "12", 79), ("7", "13", 94), ("7", "14", 99), ("7", "15", 14), ("7", "16", 47), ("7", "17", 90), ("7", "18", 15), ("7", "19", 46), ("8", "9", 78), ("8", "10", 69), ("8", "11", 42), ("8", "12", 66), ("8", "13", 95), ("8", "14", 10), ("8", "15", 37), ("8", "16", 89), ("8", "17", 73), ("8", "18", 83), ("8", "19", 47), ("9", "10", 20), ("9", "11", 42), ("9", "12", 63), ("9", "13", 19), ("9", "14", 83), ("9", "15", 6), ("9", "16", 62), ("9", "17", 53), ("9", "18", 42), ("9", "19", 52), ("10", "11", 84), ("10", "12", 52), ("10", "13", 30), ("10", "14", 14), ("10", "15", 27), ("10", "16", 58), ("10", "17", 76), ("10", "18", 31), ("10", "19", 5), ("11", "12", 0), ("11", "13", 61), ("11", "14", 49), ("11", "15", 13), ("11", "16", 65), ("11", "17", 24), ("11", "18", 70), ("11", "19", 90), ("12", "13", 70), ("12", "14", 60), ("12", "15", 25), ("12", "16", 81), ("12", "17", 99), ("12", "18", 51), ("12", "19", 29), ("13", "14", 65), ("13", "15", 63), ("13", "16", 28), ("13", "17", 30), ("13", "18", 57), ("13", "19", 23), ("14", "15", 63), ("14", "16", 81), ("14", "17", 4), ("14", "18", 66), ("14", "19", 40), ("15", "16", 48), ("15", "17", 82), ("15", "18", 72), ("15", "19", 28), ("16", "17", 17), ("16", "18", 30), ("16", "19", 51), ("17", "18", 99), ("17", "19", 34), ("18", "19", 29)]

gTest15 :: RoadMap
gTest15 = [("0", "1", 24), ("0", "2", 12), ("0", "3", 47), ("0", "4", 31), ("0", "5", 26), ("0", "6", 54), ("0", "7", 35), ("0", "8", 29), ("0", "9", 18), ("0", "10", 32), ("0", "11", 37), ("0", "12", 44), ("0", "13", 22), ("0", "14", 28),
           ("1", "2", 19), ("1", "3", 40), ("1", "4", 33), ("1", "5", 17), ("1", "6", 45), ("1", "7", 22), ("1", "8", 37), ("1", "9", 29), ("1", "10", 25), ("1", "11", 31), ("1", "12", 21), ("1", "13", 39), ("1", "14", 36),
           ("2", "3", 38), ("2", "4", 16), ("2", "5", 23), ("2", "6", 40), ("2", "7", 36), ("2", "8", 18), ("2", "9", 20), ("2", "10", 33), ("2", "11", 27), ("2", "12", 30), ("2", "13", 34), ("2", "14", 25),
           ("3", "4", 22), ("3", "5", 28), ("3", "6", 42), ("3", "7", 18), ("3", "8", 35), ("3", "9", 25), ("3", "10", 24), ("3", "11", 38), ("3", "12", 27), ("3", "13", 20), ("3", "14", 30),
           ("4", "5", 31), ("4", "6", 15), ("4", "7", 39), ("4", "8", 20), ("4", "9", 32), ("4", "10", 23), ("4", "11", 21), ("4", "12", 29), ("4", "13", 17), ("4", "14", 36),
           ("5", "6", 27), ("5", "7", 32), ("5", "8", 21), ("5", "9", 19), ("5", "10", 35), ("5", "11", 16), ("5", "12", 25), ("5", "13", 30), ("5", "14", 33),
           ("6", "7", 26), ("6", "8", 24), ("6", "9", 34), ("6", "10", 22), ("6", "11", 18), ("6", "12", 40), ("6", "13", 28), ("6", "14", 23),
           ("7", "8", 19), ("7", "9", 27), ("7", "10", 21), ("7", "11", 25), ("7", "12", 32), ("7", "13", 31), ("7", "14", 20),
           ("8", "9", 26), ("8", "10", 28), ("8", "11", 24), ("8", "12", 37), ("8", "13", 18), ("8", "14", 29),
           ("9", "10", 19), ("9", "11", 23), ("9", "12", 20), ("9", "13", 34), ("9", "14", 32),
           ("10", "11", 17), ("10", "12", 26), ("10", "13", 33), ("10", "14", 30),
           ("11", "12", 22), ("11", "13", 27), ("11", "14", 28),
           ("12", "13", 31), ("12", "14", 25),
           ("13", "14", 24)]