addOne2List :: [Int] -> [Int]
addOne2List [] = []
addOne2List lst = map(+1) lst

maybeIncrement :: Maybe Int -> Maybe Int
maybeIncrement Nothing = Nothing
maybeIncrement x = fmap(+1) x

incrementContents :: Functor f => f Int -> f Int
incrementContents Nothing = Nothing
incrementContents Just lst = fmap(+1) lst


convertStringList2NumList lst = map read lst
