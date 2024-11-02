
# PFL - Haskell Coursework


## Shortest Path

### Algorithm Selection

The first step to complete this task was finding what algorithm we would like to implement.

Using the knowledge we obtained about algorithms in college, we immediatly thought about the famous Dijkstra algorithm. It is a famous, well defined and simple approach to solve this problem, that we already studied and understood.

We immediatly started thinking about how to implement in a functional language like Haskell.

---


The first step to complete this task was finding what algorithm we would like to implement.

Using the knowledge we obtained about algorithms in college, we immediatly thought about the famous Dijkstra algorithm. It is a famous, well defined and simple approach to solve this problem, that we already studied and understood.

We immediatly started thinking about how to implement in a functional language like Haskell.




### Data Structures

After revisiting the algorithm logic, we understood that we had to, somehow, store some information about the different nodes of the graph. We had to know whether a city was already visited or not, it's distance to the city node and also the parent cities

For that we created an auxiliar data structure **DijkstraList**, which is actually a matrix, so that random access is possible and in constant time. This matrix has the number of entries equal to the number of cities, where each one stores the information about a city with the number of its index.

---

After reviewing the algorithm's logic, we realized that we needed to store key information about each node in the graph. Specifically, we had to track whether each city (node) had been visited, its current shortest distance from the source city, and the preceding city (parent) on the shortest path.

To manage this data, we created an auxiliary data structure called DijkstraList. This structure is implemented as a matrix, allowing for efficient, constant-time random access. Each entry in the matrix corresponds to a specific city and stores all necessary information for that city, indexed by its unique identifier. This design enables us to quickly retrieve and update data for any city as we progress through the algorithm.

### Specifics 

#### DijkstraList Parents

In order to build a function able to return multiple paths (if there were multiple smallest ones ) we had to make it so, the third element of **DijkstraList** (that stores the parent cities), was actually a list. Able to store multiple parent nodes.

---

DijkstraList Parents
To enable our function to return multiple shortest paths (in cases where multiple paths have the same minimum distance), we modified the third element of DijkstraList—which stores parent cities—to be a list instead of a single value. This allows each city to store multiple parent nodes, thereby supporting the retrieval of all equally shortest paths.

#### Path Construction

The construction of the paths is done in the function **getDijkstraPath** by recursively going through the parent cities of each city, starting in the destiny city until the origin city is reached!

--- 

Path construction is handled by the getDijkstraPath function, which builds each path by recursively traversing through the parent cities. Starting from the destination city, the function moves backward through each city's parents until it reaches the origin city. This approach efficiently reconstructs the shortest path(s) from origin to destination.

## TSP (Travel Salesman Problem)

### Algorithm understading

For this algorithm we opted to use a simple dynamic approach to solve it, since it was mentioned in the function description of the project specifications.

In this case, we could concentrate our focus on how the algorithm worked.

The idea behind the dynamic approach is similar to the ...



## Group Members

Member Name || IdNumber || Contribution
- Eduardo Portugal || up20220628 || 50%
- Xavier Martins || up202206632 || 50%




