{---
1. Define a function dropOdds :: Int -> Int with the following behaviour.
For any positive number m, dropOdds m is got by dropping all the odd digits 
in m. (If all the digits in the number are odd, the answer should be 0.)

Test cases:
dropOdds 0 			= 0
dropOdds 8 			= 8
dropOdds 1357 	= 0
---}


--makeList: Returns List of Individual Digits from a given Number
makeList :: Int -> [Int]
makeList num
  |num==0 = []
  |otherwise = makeList (div num 10) ++ [(mod num 10)]


--makeNum: Returns Number from given List of Individual Digits 
makeNum :: [Int] -> Int
makeNum [] = 0
makeNum (x:xs) =( x * (gimmeTenPow (length (x:xs)))) + makeNum xs


--gimmeTenPow: Returns Ten power N-1 given N
gimmeTenPow :: Int -> Int
gimmeTenPow 0 = 1
gimmeTenPow n = round (10 ** fromIntegral (n-1))

--gimmeTwoPow: Returns Two power N-1 given N
gimmeTwoPow :: Int -> Int
gimmeTwoPow 0 = 1
gimmeTwoPow n = round (2 ** fromIntegral (n-1))

--droppidyOdds: Returns List with No Odd Digits
droppidyOdds ::[Int] -> [Int]
droppidyOdds [] = []
droppidyOdds (x:xs) 
  |mod x 2 ==0 = x:droppidyOdds xs
  |otherwise = droppidyOdds xs

--dropOdds: Return Number without Odd Digits
dropOdds :: Int -> Int
dropOdds n =makeNum (droppidyOdds (makeList n))





{---
2. Define a function moreZeros :: Int -> Bool such that moreZeros n returns 
True exactly when the binary representation of n has strictly more 0s than 1s.

Test cases:
moreZeros 0     = True
moreZeros 1			= False
moreZeros 2     = False
moreZeros 4			= True
---}


--countOnes: Takes a Binary List and Counts Number of Ones
countOnes :: [Int] -> Int
countOnes [] = 0
countOnes (x:xs)
  | x==1 = 1 + countOnes xs
  |otherwise = countOnes xs

--moreZeros: Checks if given Numbers Binary Representation has more Zeros than Ones
moreZeros :: Int -> Bool
moreZeros 0 = True
moreZeros n = (countOnes (toBinary n)) < (((length (toBinary n))) `quot` 2)





{---
3. Define a function binToTer :: Int -> Int which takes as input the binary 
representation of a number n and outputs the ternary representation of n. 
(You can assume that the input consists only of the digits 0 and 1, and the 
output should only consist the digits 0, 1 and 2.)

Test cases:
binToTer 0 			= 0
binToTer 1      = 1
binToTer 11     = 10
binToTer 100    = 11

---}

--toBinary: Takes a Number and converts it to Ternary List
toBinary :: Int -> [ Int ]
toBinary 0 = [ 0 ]
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

--toTernary: Takes a Number and converts it to Ternary List
toTernary :: Int -> [ Int ]
toTernary 0 = [ 0 ]
toTernary n = toTernary ( n `quot` 3 ) ++ [ n `rem` 3 ]

--toNumeric: Takes Binary List as Input and Converts to Numeric
toNumeric :: [Int] -> Int
toNumeric [] = 0
toNumeric (x:xs) = ( x * (gimmeTwoPow (length (x:xs)))) + toNumeric xs

--binToTer: Take binary and Convert it to Ternary
binToTer :: Int -> Int
binToTer 0 = 0
binToTer n = makeNum (toTernary (toNumeric (makeList n)))





{---

4. Define a function palindrome :: Int -> Bool which outputs True exactly when 
the number is a palindrome (digits read from left to right is the same as 
digits read from right to left).

Test cases:
palindrome 0		= True
palindrome 121	= True


---}

--checkPalindrome: Takes a List and Checks if its Palindrome
checkPalindrome :: [Int] -> Bool
checkPalindrome [] = True
checkPalindrome [x] = True
checkPalindrome (x:xs) 
  |(head (x:xs))==(last (x:xs)) = checkPalindrome (init (tail (x:xs)))
  |otherwise = False


--palindrome: Checks if given Number is a Palindrome
palindrome :: Int -> Bool
palindrome n = checkPalindrome (makeList n)