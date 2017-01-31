-- Copyright (c) 2016 Ahmed Amr Abdul-Fattah
--This is an Implementation for the KNN-CLassification Algorithm

import System.Environment
import Data.List
import System.IO

messages = ["KNN-CLassification Program has Started","Welcome","Please Enter K","Please Enter Testing Tuple","Final Result is "]

--Find Euclidean Distance between xs and ys
distance xs ys = sqrt (sum [((read x ::Double)-(read y ::Double)) ^ 2 | (x,y) <- (zip xs ys)])

distCompare (x,y) (x2,y2) = if x>x2 then GT else LT

distances xss xs = sortBy (distCompare) [(distance (xss!!i) xs,last (xss!!i))| i<-[0..length xss-1]]

frequency :: Ord a => [a] -> [(Int,a)] 
frequency l = zip (map length $ group $ sort l) $ nub l

vote xs k = snd (head (frequency [snd (xs!!i) | i <- [0..k]]))

knn xss xs k = vote (distances (xss) xs) (k)



main = do
    args <- getArgs
    let fileName = args!!0
    let k | length args > 1 = (read (args!!1) :: Int)
          | otherwise = 1::Int
    fileContents <- (readFile fileName)
    let line = lines fileContents
    let testingData = [words x | x<-line]
    putStrLn $ messages!!0
    putStrLn $ messages!!1
    putStrLn $ messages!!3
    l <- getLine
    let testTuple = words l
    putStrLn $ messages!!4
    putStrLn $ knn testingData testTuple k
    return 0