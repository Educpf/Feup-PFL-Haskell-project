import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.


type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]
type AdjList = [(City,[(City,Distance)])]
type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)

-- Store City information related to Dijkstra Algorithm
type DijkstraList = Data.Array.Array Int (Bool, Distance, [Int]) -- (visited, distanceFromOrigin, [PreviousCity])
-- Store calculated distances of TSP problem
type MemoizationTable = Data.Array.Array (Int, Int) (Maybe Distance)

{-|
    Auxiliar function that converts a 'RoadMap' to an Adjacent List

    Parameters:

        - 'rm' - the 'RoadMap' to convert

    Return:

        - 'AdjList' - the conversion result
    
    Complexity:

        - O(n^2 + n*m), where n is the number of elements in the roadmap and m the number of cities 

-}
convertToAdjList :: RoadMap -> AdjList
convertToAdjList rm = [(city, adjacent rm city) | city <- cities rm]

{-|
    Auxiliar function that converts a 'RoadMap' to an Adjacent Matrix

    Parameters:

        - 'rm' - the 'RoadMap' to convert

    Return:

        - 'AdjMatrix' - the conversion result 

    Complexity:

        - O(n^2 + m^2 * n), where n is the number of elements in the roadmap and m is the number of different cities in the roadmap
-}
createAdjMatrix :: RoadMap -> AdjMatrix
createAdjMatrix rm = Data.Array.array ((0,0), (ncities,ncities)) [((x,y),distance rm (show x) (show y))|  x <- [0..ncities], y <- [0..ncities]]
                    where ncities = length (cities rm) -1

{-|
    Computes the list of unique cities in a given 'RoadMap'

    Parameters:

        - 'rm' - the 'RoadMap' containing cities and their connections

    Return:

        - ['City'] - a list of the unique cities

    Complexity:

        - O(n^2), where n is the number of elements in the roadmap

-}
cities :: RoadMap -> [City]
cities = Data.List.nub . foldr (\ (c1,c2,_) acc -> c1 : c2 : acc) []

{-|
    Checks if two cities are directly connected in a given 'RoadMap'

    Parameters:

        - 'rm' - the 'RoadMap' containing cities and their connections

        - 'c1' - The first 'City' 

        - 'c2' - The second 'City'

    Return:

        - 'Bool' - 'True' if the cities are connected and 'False' otherwise 

    Complexity:

        - O(n), where n is the number of elements in the roadmap

-}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = any (\(x,y,_) -> (c1==x && c2==y) || (c2==x && c1==y)) rm


{-|
    Compute the distance between two cities if they are directly connected

    Parameters:

        - 'rm' - the 'RoadMap' containing cities and their connections

        - 'c1' - The first 'City' 

        - 'c2' - The second 'City'

    Return:

        - 'Maybe' 'Distance' - 'Nothing' if the cities are not directly connected.
            Otherwise, 'Just' 'x', where 'x' is the distance between the cities

    Complexity:

        - O(n), where n is the number of elements in the roadmap

-}
distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2 = fmap (\(_,_,d) -> d) (Data.List.find (\(x,y,_) -> (c1==x && c2==y) || (c2==x && c1==y)) rm)

{-|
    Compute the adjacent cities and respective distances for a given 'City' in a 'RoadMap'

    Parameters:

        - 'rm' - the 'RoadMap' containing cities and their connections

        - 'c' - the 'City' to analyse

    Return:

        - [(City, Distance)] - list of tuples, where each one represents a connection to an adjacent city and contains:
        -- A 'City' - the adjacent city
        -- A 'Distance' - the distance to that city

    Complexity:

        - O(n), where n is the number of elements in the roadmap

-}
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent rm c = [ if  x == c then (y,d) else (x, d) | (x,y,d) <- rm, x == c || y == c]

{-|
    Computes the total distance of a 'Path', by iteratively adding up the distance of a city to the next 

    Parameters:

        - 'rm' - the 'RoadMap' containing cities and their connections

        - 'p' - the 'Path' to calculate

    Return:

        - 'Maybe' 'Distance' - 'Nothing' if 'Path' is invalid for the given 'RoadMap'
            Otherwise, 'Just' 'x', where 'x' is the total length of the 'Path'


    Complexity:

        - O(n * m), where n is the number of elements in the roadmap and m is number of elements in the path

-}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance rm p = foldr (\ v acc -> case (v,acc) of
                                            (Nothing,_) -> Nothing
                                            (_,Nothing) -> Nothing
                                            (Just x,Just tot) -> Just (x + tot) ) (Just 0) [distance rm x y | (x,y) <- zip p (tail p)]

{-|
    Computes the cities with the highest number of roads connecting to them

    Parameters:

        - 'rm' - the 'RoadMap' containing cities and their connections

    Return:

        - ['City'] - list of cities, where each one represents a 'City' with the highest calculated degree

    Complexity:

        - O(n * m), where n is the number of elements in the roadmap and m is the number of unique cities in the roadmap

-}
rome :: RoadMap -> [City]
rome rm = [ c | (c, d) <- cityDegree, d == maxDegree ]
        where
            cityDegree = map (\(c, l)-> (c, Data.List.length l)) (convertToAdjList rm)
            maxDegree = Data.List.maximum (map snd cityDegree)

{-|
    Computes the cities that can be visited from a given city

    Parameters:

        - 'al' - the 'AdjList' containing cities and their connections to other cities, including the distance

        - 'c' -  the city to analyse

        - 'visited' - list with the visited cities, in order to keep track of them

    Return:

        - ['City'] - list of cities, which are the cities that the dfs can reach from a starting city (visited cities)

    Complexity:
    
        - O(n + m), where n is the number of cities and m is the number of connections between them

-}
dfs :: AdjList -> City -> [City] -> [City]
dfs al c visited    |  c `elem` visited = visited
                    | otherwise = foldr (dfs al) (c : visited) children
        where
            children =  head [ map fst d | (city, d) <- al, city == c]

{-|
    Checks if a roadmap is strongly connected, if every city can reach every other city

    Parameters:

        - rm - the 'RoadMap' containing cities and their connections

    Return:

        - 'Bool' - 'True' if the roadmap is strongly connected and 'False' otherwise 


    Complexity:

        - O(n^2 + n * m), where n is the number of elements in the roadmap and m is the number of unique cities in the roadmap 

-}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = length (dfs (convertToAdjList rm) (head (cities rm)) []) == totalCity
                        where totalCity = length (cities rm)


-- SHORTEST PATH

{-|
    Auxiliar function that creates a 'DijkstraList' with all values initialized to (False, maxbound, []), except the origin that is initialized as (False, 0, [])

    Parameters:

        - 'len' - the size that the table should have (number of cities)

        - 'origin' - the origin node of the shortest path

    Return:

        - 'DijkstraList', a list with all cities(as index) storing visited state, distance from origin and a list with previous city

    Complexity:

        - O(n), where n is the len passed as argument, representing the number of cities of the roadmap

-}
createDijkstraTable :: Int -> Int -> DijkstraList
createDijkstraTable len origin = Data.Array.array (0,len) [if i == origin then (i,(False, 0, [])) else(i,(False, maxBound, [])) | i <- [0..len]]


{-|
    Auxiliar function that computes the Dijkstra path from a given 'DijkstraList' and starting point

    Parameters:

        - list - 'DijkstraList' to perform the search of the path

        - dest - Destination node of the path 

    Return:

        - [[City]], list of lists of cities, representing the various possible paths from an origin to a destination

    Complexity:

        - O(n) where n is the minimal length of the path that connects the cities, stored in the DijkstraList

-}
getDijkstraPath :: DijkstraList -> Int -> [[City]] -- Use cities as Int
getDijkstraPath list dest  | null back = [[show dest]]
                            | otherwise =  map ( ++ [show dest] ) (foldr (\elem acc -> acc ++ getDijkstraPath list elem) [] back)
                                where (visited,_,back) = list Data.Array.! dest


{-|
    Auxiliar function that selects, for a given 'DijkstraList' what is the unvisited 'City' with the smallest distance from the origin

    Parameters:

        - list - 'DijkstraList' to perform the search for the Node

    Return:

        - 'Int' - the selected City. Returns -1 if all cities are already visited.

    Complexity:

        - O(n) - where 'n' is the number of Cities stored in the 'DijkstraList'
-}
getSmallerUnvisited :: DijkstraList -> Int
getSmallerUnvisited list = result
                where (_, result, _) = foldl (\(i, bi, bd) (v, d, _)  -> if not v && (d < bd) then (i+1, i, d) else (i+1, bi, bd)) (0, -1, maxBound) (Data.Array.elems list)


{-|
    Auxiliar function that, given an origin and destiny city, updates the destiny city entry in a 'DijkstraList', based on the connection between the two

    Parameters:

        - 'matrix' - the 'AdjMatrix' with the city connections

        - 'list' - the 'DijkstraList' to update

        - 'origin' - the origin 'City' as 'Int'

        - 'dest' - the destiny 'City' as 'Int'

    Return:

        - '(Bool, Distance, [Int])' - the updated destiny city entry of the 'DijkstraList'

    Complexity:

        - 'O(1)'
-}
updateConnection :: AdjMatrix -> DijkstraList -> Int -> Int -> (Bool, Distance, [Int]) -- Only one element
updateConnection matrix list origin dest | origin == dest = (True, d_o, p_o)   -- comparing the same city, just mark it as visited
                                         | distance == Nothing = (v_d,d_d,p_d)  -- cities are not connected, don't change entry
                                         | newDistance <  d_d = (v_d, newDistance, [origin]) -- better connection, overwrite distance and parent city
                                         | newDistance ==  d_d = (v_d, d_d, origin : p_d) -- connection as good, add origin as new parent city
                                         | otherwise = (v_d, d_d, p_d) -- worst connection, don't change entry
                                            where
                                                (v_o,d_o,p_o) = list Data.Array.! origin
                                                (v_d,d_d,p_d) = list Data.Array.! dest
                                                distance = matrix Data.Array.! (origin, dest)
                                                newDistance = case distance of
                                                                Nothing -> d_o
                                                                Just num -> num + d_o


{-|
    Auxiliar function that, given an origin city, updates all the 'DijkstraList' entries

    Parameters:

        - 'matrix' - the 'AdjMatrix' with the city connections
        - 'list' - the 'DijkstraList' to update
        - 'city' - the origin 'City' as 'Int'

    Return:

        - 'DijkstraList' - the updated 'DijkstraList' based on the given 'City'

    Complexity:

        - 'O(n)' - where 'n' is the total number of cities
-}
updateConnections :: AdjMatrix -> DijkstraList -> Int -> DijkstraList
updateConnections matrix list city = Data.Array.array (0, maxI) [ (i, updateConnection matrix list city i) | i <- [0..maxI]]
                            where (_, maxI)= Data.Array.bounds list


{-|
    Auxiliar function that computes all the shortest paths, using Dijkstra Algorithm, between an origin and destiny cities of a given 'AdjMatrix'

    Parameters:

        - 'matrix' - the 'RoadMap' containing cities and their connections
        - 'list' - the 'DijkstraList' used to store algorithm data
        - 'origin'- the destiny 'City' as 'Int'
        - 'destiny'- the destiny 'City' as 'Int'

    Return:

        - '[Path]' - the list of all possible different paths connecting both cities

    Complexity:

        - 'O(n^2)' - where 'n' is the total number of cities
-}
dijkstra :: AdjMatrix -> DijkstraList -> Int -> Int -> [Path]
dijkstra matrix list origin destiny | nextNode == -1 = getDijkstraPath list destiny -- all cities visited, compute path
                                    | otherwise = dijkstra matrix updatedList origin destiny -- otherwise, recursively call the function
                                    where
                                        nextNode = getSmallerUnvisited list -- get next city to process
                                        updatedList = updateConnections matrix list nextNode -- update DijkstraList based on the selected city


{-|
    Computes all the shortest paths between an origin and destiny cities of a given 'RoadMap'

    Parameters:

        - 'rm' - the 'RoadMap' containing cities and their connections
        - 'origin' - the origin 'City'
        - 'destiny'- the destiny 'City'

    Return:

        - '[Path]' - the list of all possible different paths connecting both cities

    Complexity:

        - 'O(n^2)' - where 'n' is the total number of cities
-}
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm origin destiny = dijkstra adjMatrix (createDijkstraTable numCities (read origin)) (read origin) (read destiny) -- get Paths with dijkstra Algorithm
                                    where adjMatrix = createAdjMatrix rm
                                          (_, numCities) = snd (Data.Array.bounds adjMatrix)

-- TRAVEL SALES


{-|
    Auxiliar function to get the number of cities in a given 'AdjMatrix'

    Parameters:

        - 'matrix' - the 'AdjMatrix' of the connections bettwen cities

    Return:

        - 'Int' - the number of cities

    Complexity:

        - 'O(1)' 
-}
getNumCities :: AdjMatrix -> Int
getNumCities matrix = snd (snd (Data.Array.bounds matrix)) + 1


{-|
    Auxiliar function that recursively fills up a given 'MemoizationTable' for TSP, for a given 'AdjMatrix'

    Parameters:

        - 'matrix' - the 'AdjMatrix' of the connections bettwen cities
        - 'table' - the 'MemoizationTable' to fill 
        - 'visited' - an 'Int', where each bit represents whether a city is visited or not
        - 'origin' - the current City being processed

    Return:

        - 'MemoizationTable' - the result table

    Complexity:

        - 'O(n^2 * 2^n)' - where n is the number of cities in AdjMatrix
-}
fillTable :: AdjMatrix -> MemoizationTable -> Int -> Int -> MemoizationTable
fillTable matrix table visited origin | storedValue /= Nothing = table -- Value already stored in table, return table
                                      | Data.Bits.shiftL 1 numCities - 1 == updatedVisited = table Data.Array.// [((visited, origin), distanceToZero)] -- all visited besides current
                                      | otherwise = updatedTable Data.Array.// [((visited, origin), Just distance)] -- Get table updated by descendant and add entry 
                                        where
                                            numCities = getNumCities matrix
                                            -- Get value stored in memoizationTable for current position
                                            storedValue = table Data.Array.! (visited, origin)
                                            -- Make current node visited
                                            updatedVisited = visited Data.Bits..|. Data.Bits.shiftL 1 origin
                                            distanceToZero = matrix Data.Array.! (origin, 0)
                                            -- Get unvisited nodes connected to current node
                                            nextNodes = [i | i <- [0..numCities - 1], (Data.Bits.shiftL 1 i Data.Bits..&. updatedVisited) == 0, (matrix Data.Array.! (origin, i)) /= Nothing]
                                            -- Get memoizationTable updated by descendants and the one with the smaller distance (the best one)
                                            (updatedTable, distance, nextCity) = foldl (\(accTable, bestDistance, bestI) elem ->  -- Recursively update table and find best descendant
                                                                                                        let
                                                                                                            updatedTable = fillTable matrix accTable updatedVisited elem -- table updated by child city
                                                                                                            distanceFromNode = updatedTable Data.Array.! (updatedVisited, elem) -- distance from the child city forward
                                                                                                            distanceToNode = matrix Data.Array.! (origin, elem)  -- distance between current city and the child
                                                                                                            totalDistance = case (distanceFromNode, distanceToNode) of -- handle nothing and maxBound values
                                                                                                                        (Nothing, _) -> maxBound
                                                                                                                        (_, Nothing) -> maxBound
                                                                                                                        (Just d1, Just d2) -> if distanceFromNode == Just maxBound then maxBound else d1 + d2
                                                                                                            in if totalDistance < bestDistance then (updatedTable,totalDistance,elem) else (updatedTable, bestDistance,bestI)) (table, maxBound, -1) nextNodes





{-|
    Auxiliary function that creates an empty 'MemoizationTable'
    

    Parameters:

        - 'size' - the number of cities to store in MemoizationTable as 'Int' 

    Return:

        - 'MemoizationTable' - the created table, with all values as 'Nothing' with dimensions '(2^size)-1'x'size-1'

    Complexity:

        - O(n^2 * n) - where n is the size passed as argument, representing the number of cities

-}
createMemoizationTable :: Int -> MemoizationTable
createMemoizationTable size = Data.Array.array ((0, 0), (maxRow, maxColumn)) [ ((x, y), Nothing) | x <- [0..maxRow], y <- [0 .. maxColumn]]
                                where
                                    maxColumn = size - 1
                                    maxRow = (2 ^ size) -1



{-|
    Auxiliary function that returns a TSP based on a given 'MemoizationTable'
    
    Parameters:

        - 'matrix' - the 'AdjMatrix' of the connections bettwen cities
        - 'memoTable' - the filled 'MemoizationTable' 
        - 'visited' - an 'Int', where each bit represents whether a city is visited or not
        - 'currentCity' - the city being currently analysed
    Return:

        - 'Path' - a possible path that solves the TSP for the given matrix

    Complexity:

        - O(n^2) - where n is the total number of cities in the AdjMatrix

-}
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



{-|
    Function that returns a path that solves the TSP problem for a given RoadMap

    Parameters:

        - 'rm' - the 'RoadMap' containing cities and their connections

    Return:

        - 'Path' - a path of cities that solves the tsp

    Complexity:

        - 'O(n^2 * 2^n)', where n is the total number of cities in the AdjMatrix
-}
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