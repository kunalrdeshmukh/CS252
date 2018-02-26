fizzbuzz :: Int -> String
fizzBuzz n =
        if n 'mod' 3 == 0 && n 'mod' 5 == 0 then
            putStrLn "fizzbuzz"
        else if n 'mod' 3 == 0 then
            putStrLn "fizz"
        else if n 'mod' 5 == 0 then
            putStrLn "buzz"
        else
            print n
    