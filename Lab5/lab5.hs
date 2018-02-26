
getMax [] = Nothing
getMax (x:xs)  | Just x > getMax(xs) = Just x
               | otherwise = getMax(xs)


reciprocal :: (Eq a, Fractional a) => a -> Maybe a
reciprocal 0 = Nothing
reciprocal x = Just (1 / x)


rectangleArea :: Int -> Int -> Either String Int
rectangleArea x y | x<0 = Left "Width is not positive"
                  | y<0 = Left "Height is not positive"
                  | otherwise = Right $ x*y


isPalindrome x | length x == 0  = True
               | length x == 1  = True
isPalindrome (x:xs) | x == last xs =  isPalindrome $ init xs
                    | otherwise = False
