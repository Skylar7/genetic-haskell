module Tree
( makePopulation
) where

import System.Random
import Data.List
import Data.Bits

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq,Show)
data Value a b = Operator a | Terminal b deriving (Show, Eq, Ord, Read) 

type Leaf = Value Char Double
type ExpTree = Tree Leaf --ExpTree stands for "Expression Tree"

type Point = (Double, Double)
type RandList = [Double]

acceptableError = 1.0
randList = take 20000 $ randomRs (1,20) (mkStdGen 69) :: [Double]
points = ["1 3", "2 4", "3 5", "6 7.5", "14.2 16.69"]

makeTerm :: Double -> Leaf
makeTerm x = Terminal x

makeOp :: Char -> Leaf
makeOp c = Operator c

getTerm :: Leaf -> Double
getTerm (Terminal x) = x

getOp :: Leaf -> Char
getOp (Operator c) = c

randOpList :: Int -> RandList -> ([Char], RandList)
randOpList _ [] = ([],[]) 
randOpList 0 xs = ([],xs)
randOpList n (x:xs)
 | mod (ceiling x) 5 == 0 = ('+' : otherOp, remainingRandList) 
 | mod (ceiling x) 5 == 1 = ('-' : otherOp, remainingRandList)
 | mod (ceiling x) 5 == 2 = ('*' : otherOp, remainingRandList) 
 | mod (ceiling x) 5 == 3 = ('/' : otherOp, remainingRandList) 
 | mod (ceiling x) 5 == 4 = ('+' : otherOp, remainingRandList) 
  where otherOp = fst $ randOpList (n - 1) xs
        remainingRandList = snd $ splitAt n (x:xs)

randTermList :: Int -> RandList -> RandList -> ([Double], RandList)
randTermList _ _ [] = ([],[])
randTermList _ [] _ = ([],[])
randTermList 0 xs _ = ([],xs)
randTermList n (x:xs) (y:ys)
 | even (ceiling y) = (0 : otherTerm, remainingTermList)
 | otherwise = (x : otherTerm, remainingTermList)
  where otherTerm = fst $ randTermList (n - 1) xs ys
        remainingTermList = snd $ splitAt n (x:xs)

ops :: Int -> RandList -> [Leaf]
ops n xs = map makeOp . fst $ randOpList n xs

terms :: Int -> RandList -> RandList -> [Leaf]
terms n xs ys = map makeTerm . fst $ randTermList n xs ys

listTree :: Int -> RandList -> ([Leaf],[Leaf]) --Int is depth of tree
listTree n xs = (terms numTerms tList bList, ops numOps oList)
 where numTerms = 2 ^ m
       numOps = (2 ^ m) - 1
       (oList, rest) = splitAt third xs
       (tList, bList) = splitAt (length rest `div` 2) rest  
       third = (length xs) `div` 3
       m = n - 1

fullTree :: ([Leaf],[Leaf]) -> ExpTree
fullTree (t:[],[]) = Node (t) Empty Empty
fullTree (ts,os)
 |endOpsBranch = Node (currentOp) (fullTree (leftTerms,[])) (fullTree (rightTerms,[]))
 |otherwise = Node (currentOp) (fullTree (leftTerms, leftOps)) (fullTree (rightTerms, rightOps))
  where endOpsBranch = (==) (tail os) [] --if is single entry
        (leftOps,rightOps) = splitAt (length remOp `div` 2) remOp
        (leftTerms,rightTerms) = splitAt (length ts `div` 2) ts
        currentOp = head os
        remOp = tail os

makeTree :: Int -> RandList -> ExpTree --depth is the Int
makeTree depth xs = fullTree $ listTree depth xs

makePopulation :: Int -> [Int] -> RandList -> [ExpTree] --num of trees, max depth are the Int
makePopulation n (d:ds) xs
 | n == 0 = []
 | length xs < (2 ^ d - 1) = []
 | otherwise = makeTree d xs : makePopulation (n - 1) (d:ds) rem
  where rem = snd $ splitAt numNodes xs
        numNodes = 2 ^ d - 1

evalTreeSingle :: Double -> ExpTree -> Double
evalTreeSingle x (Node (Terminal z) (Empty) (Empty)) = if z == 0.0 then x else z
evalTreeSingle x (Node (Operator o) (left) (right))
 | o == '+' = (+) (evalTreeSingle x left) (evalTreeSingle x right)
 | o == '-' = (-) (evalTreeSingle x left) (evalTreeSingle x right)
 | o == '*' = (*) (evalTreeSingle x left) (evalTreeSingle x right)
 | o == '/' = (/) (evalTreeSingle x left) (evalTreeSingle x right)
 | o == '^' = (**) (evalTreeSingle x left) (evalTreeSingle x right)

evalTree :: [Double] -> ExpTree -> [Double]
evalTree [] _ = []
evalTree (x:xs) tree = evalTreeSingle x tree : evalTree xs tree

err :: [Double] -> [Double] -> Double
err xs ys = sum $ map abs (zipWith (-) xs ys) 

inputs :: [String] -> [Double]
inputs [] = []
inputs (x:xs) = (read converted :: Double) : inputs xs
 where converted = takeWhile (/=' ') x

outputs :: [String] -> [Double]
outputs [] = []
outputs (x:xs) = (read converted :: Double) : outputs xs
 where converted = tail $ dropWhile (/=' ') x

cull :: [ExpTree] -> [Double] -> [ExpTree] -- [Double] is list of errors for each tree
cull trees errors = fst . unzip . sortBy compareSecond $ filter (\(t,errs) -> errs < avgErr) list
 where list = zip trees errors
       avgErr = sum errors / (genericLength errors)
       compareSecond (x,xs) (y,ys) = if ys > xs then GT else LT 

generationInitial :: Int -> (Int,Int) -> [String] -> RandList -> ([ExpTree], RandList)
generationInitial trees (minDepth, maxDepth) points xs = (cull forest . map (err outs) $ map (evalTree ins) forest, rem)
 where forest = makePopulation trees (cycle [minDepth..maxDepth]) xs
       outs = outputs points
       ins = inputs points
       rem = snd $ splitAt (trees * 2 ^ ((minDepth + maxDepth) `div` 2)) xs
--Remember to test these both out thoroughly tomorrow (crossover and mutate)
crossover :: ExpTree -> ExpTree -> RandList -> (ExpTree,RandList)
crossover (Node (Terminal t1) Empty Empty) bogus (x:xs) = (bogus, xs) 
crossover bogus (Node (Terminal t1) Empty Empty) (x:xs) = (bogus, xs)
crossover (Node (o1) (left1) (right1)) (Node (o2) (left2) (right2)) (x:xs)
 | mod (ceiling x) 16 == 1 = ((Node (o1) (left1) (right2)), xs) 
 | mod (ceiling x) 16 == 2 = ((Node (o1) (left2) (right1)), xs)
 | mod (ceiling x) 16 == 3 = ((Node (o2) (left1) (right2)), xs)
 | mod (ceiling x) 16 == 4 = ((Node (o2) (left2) (right1)), xs)
 | mod (ceiling x) 16 == 5 = crossover (left1) (Node (o2) (left2) (right2)) xs 
 | mod (ceiling x) 16 == 6 = crossover (right1) (Node (o2) (left2) (right2)) xs 
 | mod (ceiling x) 16 == 7 = crossover (left2) (Node (o1) (left1) (right1)) xs 
 | mod (ceiling x) 16 == 8 = crossover (right2) (Node (o1) (left1) (right1)) xs 
 | mod (ceiling x) 16 == 9 = crossover (left1) (left2) xs 
 | mod (ceiling x) 16 == 10 = crossover (left1) (right2) xs
 | mod (ceiling x) 16 == 11 = crossover (right1) (left2) xs
 | mod (ceiling x) 16 == 12 = crossover (right1) (right2) xs
 | mod (ceiling x) 16 == 13 = crossover (left2) (left1) xs
 | mod (ceiling x) 16 == 14 = crossover (left2) (right1) xs
 | mod (ceiling x) 16 == 15 = crossover (right2) (left1) xs
 | mod (ceiling x) 16 == 16 = crossover (right2) (right1) xs

mutate :: ExpTree -> RandList -> (ExpTree, RandList)
mutate (Node (Terminal t) Empty Empty) (x:xs)
 | even (ceiling (x + t)) = (Node (makeTerm x) Empty Empty, xs)
 | otherwise = (Node (Terminal t) Empty Empty, xs)
mutate (Node (Operator o) (left) (right)) (x:xs)
 |mod (ceiling x) 40 `elem` [1..10] = (Node (Operator o) (newBranch) (right), rem)
 |mod (ceiling x) 40 `elem` [11..20] = (Node (Operator o) (left) (newBranch), rem)
 |mod (ceiling x) 40 `elem` [30..39] =  mutate (Node (Operator o) (left) (right)) xs
  where newBranch = makeTree twoOrThree xs 
        twoOrThree = (mod (ceiling x) 2) + 2
        rem = snd $ splitAt ((2 ^ twoOrThree) - 1) xs

makeNewGen :: ([ExpTree],RandList) -> ([ExpTree], RandList)
makeNewGen (t1:t2:ts, xs) = (t1 : t2 : newTrees, rem)
 where newTrees = fst $ genHelper (t1:t2:ts, xs)
       rem = snd $ genHelper (t1:t2:ts, xs)

genHelper :: ([ExpTree], RandList) -> ([ExpTree], RandList)
genHelper (t1:t2:ts, x:xs)
 |mod (ceiling x) 100 `elem` [1..85] = (cross : treeCross, xs \\ totalRemCross)
 |mod (ceiling x) 100 `elem` [86..100] = (mut : mutCross, xs \\ totalRemMut)
 where (cross, remCross) = crossover t1 t2 xs
       (treeCross, totalRemCross) = genHelper (ts, remCross)
       (mut, remMut) = mutate t1 xs
       (mutCross, totalRemMut) = genHelper ((t2:ts), remMut)
{-
finish :: [ExpTree] -> [String] -> [ExpTree]
finsh [] _ = []
finish (x:xs) points = if err outs (evalTree ins x) < acceptableError then [x] else finish xs points
    where ins = inputs points
          outs = outputs points

loop :: [ExpTree] -> [String] -> ExpTree
loop trees points = if finish trees points == Nothing then
-}
{- Try to recycle this code to test each new generation
generationInitial trees (minDepth, maxDepth) points xs = (cull forest . map (err outs) $ map (evalTree ins) forest, rem)
 where forest = makePopulation trees (cycle [minDepth..maxDepth]) xs
       outs = outputs points
       ins = inputs points
       rem = snd $ splitAt (trees * 2 ^ ((minDepth + maxDepth) `div` 2)) xs
-}
{-       
genFunction :: [String] -> RandList -> String
genFunction str nums = randOpList nums 
-}
