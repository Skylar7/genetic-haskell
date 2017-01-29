import System.Environment
import System.Directory
import System.IO
import System.Random
import Control.Monad
import Data.List
import Tree

main = do
 args <- getArgs
 gen <- getStdGen
 if not $ null args
  then do
  let fileName = head args
  fileExists <- doesFileExist fileName
  if fileExists
   then do
   handle <- openFile fileName ReadMode
   contents <- hGetContents handle
   let points = lines contents
       rands = take 1000 $ randomRs (1,50) gen :: [Double]
   print points
--   putStrLn $ genFunction points rands
  else do putStrLn "That file doesn't exist" 

 else do
  putStrLn "Specify a file"
  fileName <- getLine
  fileExists <- doesFileExist fileName
  if fileExists
   then do
   handle <- openFile fileName ReadMode
   contents <- hGetContents handle
   let points = lines contents
       rands = take 1000 $ randomRs (1,50) gen :: [Double]
   print points
--   putStrLn $ genFunction points rands
  else do putStrLn "That file doesn't exist" 

