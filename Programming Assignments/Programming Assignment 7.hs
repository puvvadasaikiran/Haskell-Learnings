{---
The input to the program will be multiple lines. Each input line is guaranteed 
to be a single word, with no beginning or trailing spaces. On reading a line, 
your program should print "Y"  if the word is a palindrome (after converting 
every letter to lowercase), and "N" if not. 

A palindrome is a string that is the same as its reverse.

Here is a sample run:

Input
-----
abba
Level
Hi
Malayalam 

Output
------
Y
Y
N
Y

---}



import Data.Char

checkPalindrome :: [Char] -> [Char]
checkPalindrome [] = "Y"
checkPalindrome [x] = "Y"
checkPalindrome (x:xs) 
  |(head (x:xs))==(last (x:xs)) = checkPalindrome (init (tail (x:xs)))
  |otherwise = "N"


ntimes :: Int -> IO () -> IO ()
ntimes 0 _ = return ()
ntimes n a =  do {
a;
ntimes (n-1) a
}

makeMeLower :: [Char] -> [Char]
makeMeLower [] = []
makeMeLower (x:xs) = (Data.Char.toLower x ): makeMeLower xs


main = ntimes 4 act
  where 
    act = do {
      inp <- getLine;
      putStrLn (checkPalindrome (makeMeLower inp));
    }