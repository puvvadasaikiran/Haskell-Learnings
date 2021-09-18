{---
1. Write a function f1 :: [a] -> Int -> [a] that takes a list and a positive number n, and drops elements of the list at index i*n-1, for i > 0. 
Remember that list indices start at 0.

Test cases:  	
f1 [1,2,3,4,5] 2 = [1,3,5]
f1 [2,4,6,8,10] 1 = []
f1 [1,3,5,7,9,11,13,15] 4 = [1,3,5,9,11,13]

---}

eliminate :: [Int] -> Int -> Int -> Int -> [Int]
eliminate [] _ _ _= []
eliminate (x:xs) cIndex n i
  |cIndex == ((n*i)-1) = eliminate xs (cIndex+1) n (i+1)
  |otherwise = x:(eliminate xs (cIndex+1) n (i))

--- Eliminate skips the the Adding elemnents to the list based on index
--- Acts as helper function to f1

f1 :: [Int] -> Int -> [Int]
f1 [] _ = []
f1 (x:xs) n = eliminate (x:xs) 0 n 1




{---

2. Define a function f2 :: Int -> Int that takes a non-negative integer and places its rightmost digit at the leftmost position.

Test cases: 	
f2 1 = 1
f2 123 = 312
f2 67890 = 6789
f2 678900 = 67890
f2 678901 = 167890


---}


gimmeTenPow :: Int -> Int
gimmeTenPow 0 = 1
gimmeTenPow n = round (10 ** fromIntegral (n))

-- gimmeTenPow takes n and return 10 power n


gimmeLength :: [Int] -> Int
gimmeLength [] = 0
gimmeLength (x:xs)= 1 + gimmeLength xs

-- gimmeLength takes a Integer List and returns Length of list


gimmeArray :: Int -> [Int]
gimmeArray 0 = []
gimmeArray n = (gimmeArray (div n 10))++[(mod n 10)]

-- gimmeArray takes number and returns Array of Single Digits


gimmeFirstEle :: [Int] -> Int
gimmeFirstEle (x:xs)= x

--gimmeFirstEle takes Array and return First element of the Array


gimmeLastEle :: [Int] -> Int
gimmeLastEle [x] = x
gimmeLastEle (x:xs) = gimmeLastEle xs

--gimmeLastEle takes Array and return Last element of the Array


makeArray :: [Int] -> Int -> Int -> Int -> Int -> [Int]
makeArray (x:xs) index size firstele lastele 
  |index==1 = lastele : makeArray (x:xs) (index+1) size firstele lastele
  |index==size+1 = []
  |otherwise= x : makeArray xs (index+1) size firstele lastele

--makeArray does the job of replacing last element to first


makeNumber :: [Int] -> Int -> Int
makeNumber [] _ = 0
makeNumber _ (-1) = 0
makeNumber (x:xs) b
  |b<0 = 0
  |otherwise = (x * (gimmeTenPow b)) + makeNumber xs (b-1)

--makeNumber take Array of Single Integers and return the Number


f2 :: Int -> Int 
f2 0 = 0
f2 n = makeNumber (makeArray (gimmeArray n) 1 (gimmeLength (gimmeArray n)) (gimmeFirstEle (gimmeArray n)) (gimmeLastEle (gimmeArray n))) ((gimmeLength (gimmeArray n))-1)

-- f2 uses all the helper functions and does the job

