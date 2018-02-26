{-
  Name: Kunal Deshmukh
  Class: CS 252
  Assigment: HW1
  Date: 02/09/2018
  Description: implemented arithmatic operations for big num 
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0


bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] [] z = if z == 1 
		then [1] else []
bigAdd' (x:xs) ([]) 0 = [x] ++ bigAdd' xs [] 0
bigAdd' (x:xs) ([]) 1 | x+1 >999 = [(x+1) `mod` 1000 ] ++ bigAdd' xs [] 1 
                      | otherwise =  [(x+1)] ++ bigAdd' xs [] 0
bigAdd' ([]) (y:ys) 0 = [y] ++ bigAdd' [] ys 0
bigAdd' ([]) (y:ys) 1 | (y+1)>999 = [(y+1) `mod` 1000 ] ++ bigAdd' [] ys 1 
                      | otherwise =  [(y+1)] ++ bigAdd' [] ys 0
bigAdd' (x:[]) (y:[]) 0 | x+y >999 = [ (x + y) `mod` 1000 ] ++ bigAdd' [] [] 1 
                        | otherwise = [ x+y ]
bigAdd' (x:[]) (y:[]) 1 | x+y > 999 = [(x + y + 1) `mod` 1000 ] ++ [1] 
                        | otherwise = [ x + y + 1 ]
bigAdd' (x:xs) (y:ys) z | x + y > 999 = [(x + y + z) `mod` 1000 ] ++ (bigAdd' xs ys 1) 
                        | otherwise = [ x + y + z ] ++ ( bigAdd' xs ys 0)


bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract' x y 0


stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' [x] [y] z | z == 0 && x-y < 0 = error "Negative numbers not supported"
                       | z == 1 && (x-y-1) < 0 = error "Negative numbers not supported"
                       | z == 0 = [ x - y ]
                       | otherwise = [ (x - y - 1) ]
bigSubtract' (x:xs) (y:ys) z | x - y - z < 0 = [ (x - y - z + 1000) ] ++ (bigSubtract' xs ys 1) 
                             | otherwise = [ x - y - z] ++ (bigSubtract' xs ys 0)
bigSubtract' [x] [] z | z == 1 = [x-1]
                      | z == 0 = [x]
bigSubtract' [] [] _ = error "List is Empty !"

bigEq :: BigNum -> BigNum -> Bool
bigEq x y = if x == y then True else False 


bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply x [0] =  [0]
bigMultiply [0] _ = [0]
bigMultiply x y = bigAdd x ( bigMultiply x ( bigDec y)) 
                 
                

bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf _ [0] = [1]
bigPowerOf [0] _ = [0]
bigPowerOf x y = bigMultiply x  (bigPowerOf x ( bigDec y)) 

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]

