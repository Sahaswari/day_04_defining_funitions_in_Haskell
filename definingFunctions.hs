myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Main function 
main :: IO ()
main = do
    putStrLn "Length of [1,2,3,4,5]:"
    let lst = [1,2,3,4,5]
    putStrLn $ "Length of the list is " ++ show (myLength lst)
    