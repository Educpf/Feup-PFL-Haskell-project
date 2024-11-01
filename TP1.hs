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

{-|
    The 'convertToAdjList' function is an auxiliary function that has the goal of convert a 'RoadMap' to an Adjacent List

    Parameters:

        - 'rm' - Represents the 'RoadMap' that the function receives as input

    Return:

        - The function returns and 'AdjList'
    
    Complexity:


-}
convertToAdjList :: RoadMap -> AdjList
convertToAdjList rm = [(city, adjacent rm city) | city <- cities rm]

{-|
    The 'createAdjMatrix' function is an auxiliary function that creates a matrix from a certain 'RoadMap'

    Parameters:

        - 'rm' - Represents the 'RoadMap' that the function receives as input

    Return:

        - The function returns a 'AdjMatrix

    Complexity:


-}
createAdjMatrix :: RoadMap -> AdjMatrix
createAdjMatrix rm = Data.Array.array ((0,0), (ncities,ncities)) [((x,y),distance rm (show x) (show y))|  x <- [0..ncities], y <- [0..ncities]]
                    where ncities = length (cities rm) -1

{-|
    The 'cities' function has the goal of, for a given 'RoadMap', calculate the cities in it

    Parameters:

        - 'rm' - Represents the 'RoadMap' that the function receives as input

    Return:

        - The function returns a list of the unique cities

    Complexity:
        

-}
cities :: RoadMap -> [City]
cities = Data.List.nub . foldr (\ (c1,c2,_) acc -> c1 : c2 : acc) []

{-|
    The 'areAdjacent' function has the goal of see if two cities are directly connected for a certain 'RoadMap'

    Parameters:

        - 'rm' - Represents the 'RoadMap' that the function receives as input

        - 'c1' - The first inputed 'City' 

        - 'c2' - The second inputed 'City'

    Return:

        - The function returns a 'Bool'. 'True' if the cities are connected and 'False' otherwise

    Complexity:


-}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = or [(c1==x && c2==y) || (c2==x && c1==y) | (x,y,_) <- rm]
-- areAdjacent rm c1 c2 = any (\(x,y,_) -> (c1==x && c2==y) || (c2==x && c1==y)) rm 

{-|
    The 'distance' function has the goal of calculate the distance between two cities if they are directly connected

    Parameters:

        - 'rm' - Represents the 'RoadMap' that the function receives as input

        - 'c1' - The first inputed 'City' 

        - 'c2' - The second inputed 'City'

    Return:

        - The function returns a Maybe Distance value. 'Nothing' if the cities are not directly connected.
            Otherwise, Just x, where x is the distance between the cities

    Complexity:


-}
distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2 = fmap (\(_,_,d) -> d) (Data.List.find (\(x,y,_) -> (c1==x && c2==y) || (c2==x && c1==y)) rm)

{-|
    The 'adjacent' function has the goal of calculate the adjacent cities and respective distances for a given city

    Parameters:

        - 'rm' - Represents the 'RoadMap' that the function receives as input

        - 'c' - The inputed 'City' 

    Return:

        - The function returns a list with tuples of two arguments, the first is the city to whether it directly connects and the second one the respective distance

    Complexity:


-}
adjacent :: RoadMap -> City -> [(City,Distance)]
-- adjacent rm c = [ (y,d)  | (x,y,d) <- rm, x == c] ++ [(x,d)| (x,y,d) <- rm, y == c]
adjacent rm c = [ if  x == c then (y,d) else (x, d) | (x,y,d) <- rm, x == c || y == c]

{-|
    The 'pathDistance' function has the goal of calculate the total distance of a 'Path', by calculating the distance from a city to the next one in the path

    Parameters:

        - 'rm' - Represents the 'RoadMap' that the function receives as input

        - 'p' - The inputed 'Path' 

    Return:

        - The function returns a Maybe Distance value. 'Nothing' if some connection between cities does not exist.
             Otherwise, Just x, where x is the sum of the distances between the cities


    Complexity:


-}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance rm p = foldr (\ v acc -> case (v,acc) of
                                            (Nothing,_) -> Nothing
                                            (_,Nothing) -> Nothing
                                            (Just x,Just tot) -> Just (x + tot) ) (Just 0) [distance rm x y | (x,y) <- zip p (tail p)]

{-|
    The 'rome' function has the goal of calculate the cities with the highest number of roads connecting to them 

    Parameters:

        - 'rm' - Represents the 'RoadMap' that the function receives as input

    Return:

        - The function returns a list of cities, which are the cities with highest degree

    Complexity:


-}
rome :: RoadMap -> [City]
rome rm = [ c | (c, d) <- cityDegree, d == maxDegree ]
        where
            cityDegree = map (\(c, l)-> (c, Data.List.length l)) (convertToAdjList rm)
            maxDegree = Data.List.maximum (map snd cityDegree)

{-|
    The 'dfs' function is an auxiliary function with the goal of calculate which cities can be visited for a given 'AdjList'

    Parameters:

        - 'al' - Represents the 'AdjList' that the function receives as input

        - 'c' - Represents the current city analysing

        - 'visited' - A list with the visited cities, in order to keep track of them

    Return:

        - The function returns a list of cities, which are the cities that the dfs can reach from a starting city

    Complexity:


-}
-- array ! 2
dfs :: AdjList -> City -> [City] -> [City]
dfs al c visited    |  c `elem` visited = visited
                    | otherwise = foldr (dfs al) (c : visited) children
        where
            children =  head [ map fst d | (city, d) <- al, city == c]

{-|
    The 'isStronglyConnected' function has the goal of calculate if a 'roadMap' is strongly connected

    Parameters:

        - 'rm' - Represents the 'RoadMap' that the function receives as input

    Return:

        - The function returns a 'Bool'. 'True' if all the cities are visited by the dfs, proving that the 'RoadMap' is strongly connected.
            Otherwise, returns 'False'

    Complexity:


-}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = length (dfs (convertToAdjList rm) (head (cities rm)) []) == totalCity
                        where totalCity = length (cities rm)


-- SHORTEST PATH

{-|
    The 'createDijkstraTable' function is an auxiliary funtion with the goal of create a 'DijkstraList'. The objective is to help the execution of dijkstra algorithm.
    
    Note: 'DijkstraList' is an array with all values initialized to (False, maxbound, []), except the origin that is initialized as (False, 0, [])

    Parameters:

        - 'len' - Represents size that the table shoul have

        - 'origin' - It's the origin node of the shortest path

    Return:

        - The function returns a 'DijkstraList', properly initialized

    Complexity:


-}
createDijkstraTable :: Int -> Int -> DijkstraList
createDijkstraTable len origin = Data.Array.array (0,len) [if i == origin then (i,(False, 0, [])) else(i,(False, maxBound, [])) | i <- [0..len]]


{-|
    The 'getDijkstraPath' function is an auxiliary funtion with the purpose of, from a 'DijkstraList' completed and a destiny return the shortest path 

    Parameters:

        - 'list' - 'DijkstraList' to perform the search of the path

        - 'dest' - Destination node of the path 

    Return:

        - The function returns a list of lists of cities, which are the differents paths that exist, 
            since it can be more than one path because they can have the same distance

    Complexity:


-}
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


shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm origin destiny = dijkstra adjMatrix (createDijkstraTable numCities (read origin)) (read origin) (read destiny)
                                    where adjMatrix = createAdjMatrix rm
                                          (_, numCities) = snd (Data.Array.bounds adjMatrix)

-- TRAVEL SALES

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
createPath matrix memoTable visited currentCity     |  Data.Bits.shiftL 1 numCities - 1 == updatedVisited = show currentCity : ["0"] -- all visited Return 0
                                                    | nextCity == -1 = []
                                                    | otherwise = show currentCity : createPath matrix memoTable updatedVisited nextCity
                                                        where
                                                            numCities = snd (snd (Data.Array.bounds memoTable)) + 1
                                                            mydistance = memoTable Data.Array.! (visited, currentCity)
                                                            updatedVisited = visited Data.Bits..|. Data.Bits.shiftL 1 currentCity
                                                            nextNodes = [(i,memoTable Data.Array.! (updatedVisited, i)) | i <- [0..(numCities - 1)], (matrix Data.Array.! (currentCity, i)) /= Nothing]
                                                            connectedCities = [i | (i, Just d) <- nextNodes,
                                                                                                Just conCurrentToI <- [matrix Data.Array.! (currentCity, i)],
                                                                                                Just (d + conCurrentToI) ==  mydistance]
                                                            nextCity = if null connectedCities then -1 else head connectedCities



travelSales :: RoadMap -> Path
travelSales rm =  createPath adjMatrix (fillTable adjMatrix (createMemoizationTable (numCities+1) ) 0 0) 0 0
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

gTest15 :: RoadMap
gTest15 = [("0", "1", 24), ("0", "2", 12), ("0", "3", 47), ("0", "4", 31), ("0", "5", 26), ("0", "6", 54), ("0", "7", 35), ("0", "8", 29), ("0", "9", 18), ("0", "10", 32), ("0", "11", 37), ("0", "12", 44),
           ("1", "2", 19), ("1", "3", 40), ("1", "4", 33), ("1", "5", 17), ("1", "6", 45), ("1", "7", 22), ("1", "8", 37), ("1", "9", 29), ("1", "10", 25), ("1", "11", 31), ("1", "12", 21), 
           ("2", "3", 38), ("2", "4", 16), ("2", "5", 23), ("2", "6", 40), ("2", "7", 36), ("2", "8", 18), ("2", "9", 20), ("2", "10", 33), ("2", "11", 27), ("2", "12", 30), 
           ("3", "4", 22), ("3", "5", 28), ("3", "6", 42), ("3", "7", 18), ("3", "8", 35), ("3", "9", 25), ("3", "10", 24), ("3", "11", 38), ("3", "12", 27), 
           ("4", "5", 31), ("4", "6", 15), ("4", "7", 39), ("4", "8", 20), ("4", "9", 32), ("4", "10", 23), ("4", "11", 21), ("4", "12", 29), 
           ("5", "6", 27), ("5", "7", 32), ("5", "8", 21), ("5", "9", 19), ("5", "10", 35), ("5", "11", 16), ("5", "12", 25),
           ("6", "7", 26), ("6", "8", 24), ("6", "9", 34), ("6", "10", 22), ("6", "11", 18), ("6", "12", 40), 
           ("7", "8", 19), ("7", "9", 27), ("7", "10", 21), ("7", "11", 25), ("7", "12", 32), 
           ("8", "9", 26), ("8", "10", 28), ("8", "11", 24), ("8", "12", 37), 
           ("9", "10", 19), ("9", "11", 23), ("9", "12", 20), 
           ("10", "11", 17), ("10", "12", 26), 
           ("11", "12", 22)       
            ]