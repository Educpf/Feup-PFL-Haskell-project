
# PFL - Haskell Coursework

## Shortest Path

### Algorithm Selection

The first step to complete this task was finding what algorithm we would like to implement.

Using the knowledge we obtained about algorithms in previous years, we immediatly thought about the famous Dijkstra algorithm. It is a famous, well defined and simple approach to solve this problem, that we already studied and understood.

We immediatly started thinking about how to implement in a functional language like Haskell.

### Algorithm understading

Dijkstra's algorithm begins by setting the starting node’s distance to zero and all other nodes' distances to infinity, as they’re initially unreachable. From the start node, it examines all its neighboring nodes, updating each neighbor's distance if a shorter path is found through this route. The algorithm then moves to the node with the smallest known distance, repeating this process: checking all neighbors and updating distances as necessary.

It continues this pattern, expanding outward from the start node and always choosing the node with the smallest tentative distance next. Once all nodes have been processed, the algorithm concludes, leaving the shortest path from the start to each reachable node.


### Data Structures

After reviewing the algorithm's logic, we realized that we needed to store key information about each node in the graph. Specifically, we had to track whether each city (node) had been visited, its current shortest distance from the source city, and the preceding city (parent) on the shortest path.

To manage this data, we created an auxiliary data structure called **DijkstraList**. This structure is implemented as an array, allowing for efficient, constant-time random access. Each entry in the array corresponds to a specific city and stores all necessary information for that city. This design enables us to quickly retrieve and update data for any city as we progress through the algorithm.

### Specifics 

#### DijkstraList Parents

To enable our function to return multiple shortest paths (in cases where multiple paths have the same minimum distance), we modified the third element of **DijkstraList** (which stores parent cities) to be a list instead of a single value. This allows each city to store multiple parent nodes, thereby supporting the retrieval of all equally shortest paths.

#### Path Construction

Path construction is handled by the **getDijkstraPath** function, which builds each path by recursively traversing through the parent cities. Starting from the destination city, the function moves backward through each city's parents until it reaches the origin city. This approach efficiently reconstructs the shortest path(s) from origin to destination.

## TSP (Travel Salesman Problem)

### Algorithm understading

For this algorithm we opted to use a simple dynamic approach to solve it, since it was mentioned in the function description of the project specifications.

In this case, we could concentrate our focus on how the algorithm worked.

The idea behind the dynamic approach is similar to the brute force...

---
For this algorithm we chose a simple dynamic programming approach to solve the problem, as suggested in the function description in the project specifications.

The idea behind this dynamic approach is similar to brute force, but it optimizes by avoiding redundant calculations through memoization. Instead of recalculating the same paths multiple times, we store the results of subproblems, which allows us to achieve a significant speedup compared to a naive brute force solution.

In this case, we started at city 0 and used recursion to fill in the memoization table. The table keeps track of the minimum distances for each set of visited cities. By doing this, we can efficiently compute the shortest path to each city without revisiting cities unnecessarily.

Finally, beginning from an initial state where no cities have been visited, we follow the path to the next unvisited city with the shortest distance. This approach ensures we find the minimum path efficiently, balancing simplicity with performance.

### Data Structures


As previously mentioned, our dynamic programming approach to solve this problem involves creating a table to store computed distances for efficient retrieval. For this purpose, we designed a **MemoizationTable** structure, which serves as a matrix of arrays. This structure allows for quick, random access to previously computed distances, significantly improving data retrieval efficiency and reducing redundant calculations.

Because the space complexity of this approach is really high, we decided not to store in the table the parent nodes, but only the distance. This requires the path to be calculated as explained in the previous section, as we do in the function **createPath**

## Group Members + Distribution

Member Name || IdNumber || Contribution
- Eduardo Portugal || up20220628 || 50%

    - stronglyConnected
    - areAdjacent
    - adjacent

- Xavier Martins || up202206632 || 50%

    - distance
    - rome
    - cities
    - pathDistance

The two final functions, **shortestPath** and **TravelSales**, were developed entirely through pair programming.








