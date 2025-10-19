import Data.List
--import Test.QuickCheck
import  Data.Char

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer
double x = x+x




halfEvens :: [Int] -> [Int]
halfEvens [] = []
halfEvens (x:xs) | odd x = halfEvens xs
                 | otherwise = x `div` 2 : halfEvens xs






prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs


isVowel :: Char -> Bool
isVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' || c == 'y'


fgg :: [Int] -> [Int]
fgg [] = error "Expecting a non-empty list."
fgg [_] = []
fgg (x1:x2:xs)
	|x1==x2 && x1==0 = fgg(x2:xs)
	|x1==x2 && x1==1 = fgg(x2:xs)
	|x1==x2 && x2==1 =x2: fgg(x2:xs)
	|otherwise = x1 : fgg(x2:xs)

triple :: Integer -> Integer
triple x = x+x+x