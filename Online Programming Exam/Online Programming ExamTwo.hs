{--
1. Define a function f1 :: [a] -> [a] that takes a list l, and replicates each element (l!!i) (i+1) times. Remember that indices start at 0. 

Test cases:	
f1 [] = []
f1 [1] = [1]
f1 [1,4,7] = [1,4,4,7,7,7]
--}

f1 :: [Int] -> [Int]
f1 [] = []
f1 (x:xs) = gimmeRepA (x:xs) 1

-- gimmeRep: takes two digits n,i and returns array filled with n's of length i
gimmeRep :: Int -> Int -> [Int]
gimmeRep n i
  | i<1 = []
  | otherwise = [n] ++ gimmeRep n (i-1)

-- gimmeRepA: takes array and performs gimmeRep on every element according to their index+1 value
gimmeRepA :: [Int] -> Int -> [Int]
gimmeRepA [] _ = []
gimmeRepA [x] i = gimmeRep x i
gimmeRepA (x:xs) i= (gimmeRep x i) ++ (gimmeRepA xs (i+1))


{--
2. Define a function f2 :: String -> String -> Bool that checks if the first string is a subsequence (need not be contiguous) of the second. 

Test Cases: 	
f2 "ab" "abc" = True
f2 "ac" "abc" = True
f2 "ab" "bca" = False


--}

f2 :: String -> String -> Bool
f2 "" _ = True
f2 s1 s2 = isSub s1 s2 (length s1) (length s2)


isSub :: String -> String -> Int -> Int -> Bool
isSub s1 s2 l1 l2
    | l1 == 0 = True
    | l2 <= 0 = False
    | head s1 == head s2 = isSub (tail s1) (tail s2) (l1-1) (l2-1)
    | head s1 /= head s2 = isSub s1 (tail s2) l1 (l2-1)