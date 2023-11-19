module Permutations where

-- List -> Singly linked list with a single pointer at the head
xs = [1,2,3,4]

len :: [v] -> Integer
len [] = 0
len (x : xs) = 1 + len xs

{-
Since 2014 (Java 8) all features that are being included in Java
are mostly inspired by ml based programming languages. 
ocaml, Haskell
-}

{-
p [1,2,3] ->
    p [2,3] = [[2,3], [3,2]] (say rest)
    [1,2,3], [2,1,3], [2,3,1] 
-}


concatList :: [a] -> [a] -> [a]
concatList [] ys = ys
concatList (x : xs) ys = x : concatList xs ys

{-
f [[1,2], [3]] = g [1,2] (f [[3]]) = g [1,2] [3] = [1,2,3]
f [[3]] = g [3] [] = [3]
-}
concatLists :: [[abc]] -> [abc]
concatLists [] = []
concatLists (xs : xss) = concatList xs (concatLists xss)


placeInPos :: a -> Integer -> [a] -> [a]
placeInPos x 0 xs = x : xs
placeInPos x n [] = error "Index out of bound"
placeInPos x n (y : xs) = y : placeInPos x (n-1) xs

allPlacements :: a -> [a] -> [[a]]
allPlacements x xs = [placeInPos x n xs | n <- [0..(len xs)]]

fact 0 = 1
fact n = n * fact (n-1)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x : xs) = let
    rest = permutations xs
    in
        concatLists [allPlacements x permutation | permutation <- rest]


