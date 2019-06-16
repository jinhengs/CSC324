module EdgeGeographyGame where

import Data.List
import Data.Tuple

-- You may import useful modules here.

{- The input is a list of adjacency lists, e.g.,
   [ (0, [1, 2]) , (1, [0]) , (2, [1]), (3, []) ]
   means 0->1, 0->2, 1->0, 2->1, 3 has no outgoing edges.

   goodFirstVertices takes this input and computes the choices for the first
   vertex so the first player is destined to win.
-}
-- goodFirstVertices :: [(Int, [Int])] -> [Int]


--define a vertex to be a value and a list of edges, so input is now a list of vertices
type Vertex = (Int,[Int])

goodFirstVertices :: [Vertex] -> [Int]
--extract vertex value from Vertex type that returns true for playerMove
goodFirstVertices list = map fst (filter (\x -> playerMove x list) list)
 
playerMove :: Vertex -> [Vertex] -> Bool
--pick a starting vertex where there's no outgoing edge and you win
playerMove (_,[]) vertexList = True
playerMove (value,list) vertexList 
   --If there's a vertex connected to current vertex with no moves, then this is a wrong vertex since opponent can pick to move to that vertex
   |(filter ((==[]).snd) [getVert x vertexList | x <- list ]) /= [] = False
   --And to make sure no move the opponent makes you lose, returns on first false(opponent has a winning move)
   |otherwise = and [opponentMove (getVert x vertexList) (updateVertList x (value, list) vertexList) | x <- list]
 
opponentMove :: Vertex -> [Vertex] -> Bool
--your opponent lands on vertex with no outgoing vertex and you lose
opponentMove (_,[]) vertexList = False
opponentMove (value,list) vertexList 
   --If opponent picks a move where it's connected to a vertex with no moves, then you can pick it and make the opponent lose, so this is a good choice
   |(filter ((==[]).snd) [getVert x vertexList | x <- list ]) /= [] = True
   --Or to make sure you can at least make one move, returns on first true(player has a winning move)
   |otherwise = or [playerMove (getVert x vertexList) (updateVertList x (value, list) vertexList) | x <- list]

--Helper function to remove unique item(vertex or edge) from list
remove :: (a -> Bool) -> [a] -> [a]
remove f [] = []
remove f (x:xs)
   |(f x == True) = xs
   |otherwise = x: (remove f xs)
 
--Gets the unique vertex from the list of vertexes given the value of the vertex
getVert :: Int -> [Vertex] -> Vertex
getVert vertex list = case find ((==vertex).fst) list of
                           Just a -> a
						   
--Helper function for player moves, takes the value of the next vertex, the current vertex, and the vertex list to return the gamestate(list of vertices) after moving
--Sorta like generating next moves
--removes edge to value from the current vertex and delete that vertex from the vertex list(since we'll be working with that vertex in the next recursion)
updateVertList :: Int -> Vertex -> [Vertex] -> [Vertex]
updateVertList value (a, b) vertList = (a, remove(==value) b):(remove (==(getVert value vertList)) vertList)

