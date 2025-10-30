myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- AND operation examples using different conditional styles

-- Method 1: Using if-then-else (conditional expression)
and1 :: Bool -> Bool -> Bool
and1 a b = if a then (if b then True else False) else False

-- Method 2: Using guarded equations
and2 :: Bool -> Bool -> Bool
and2 a b
    | a && b    = True
    | otherwise = False

-- Method 3: Using case expression
and3 :: Bool -> Bool -> Bool
and3 a b = case a of
    True  -> case b of
        True  -> True
        False -> False
    False -> False

-- Method 4: Using where clause
and4 :: Bool -> Bool -> Bool
and4 a b = if result then True else False
    where result = a && b

-- Method 5: Using let expression
and5 :: Bool -> Bool -> Bool
and5 a b = let result = a && b
           in if result then True else False

-- Method 6: Pattern matching (most idiomatic for AND)
and6 :: Bool -> Bool -> Bool
and6 True True = True
and6 _    _    = False

-- OR operation examples using different conditional styles
-- Following pattern: True || _ = True, False || b = b

-- Method 1: Using if-then-else (conditional expression)
or1 :: Bool -> Bool -> Bool
or1 a b = if a then True else (if b then True else False)

-- Method 2: Using guarded equations
or2 :: Bool -> Bool -> Bool
or2 a b
    | a         = True
    | b         = True
    | otherwise = False

-- Method 3: Using case expression
or3 :: Bool -> Bool -> Bool
or3 a b = case a of
    True  -> True
    False -> case b of
        True  -> True
        False -> False

-- Method 4: Using where clause
or4 :: Bool -> Bool -> Bool
or4 a b = if result then True else False
    where result = a || b

-- Method 5: Using let expression
or5 :: Bool -> Bool -> Bool
or5 a b = let result = a || b
          in if result then True else False

-- Method 6: Pattern matching (following True || _ = True, False || b = b)
or6 :: Bool -> Bool -> Bool
or6 True  _ = True
or6 False b = b

-- Main function 
main :: IO ()
main = do
    putStrLn "Length of [1,2,3,4,5]:"
    let lst = [1,2,3,4,5]
    putStrLn $ "Length of the list is " ++ show (myLength lst)
    
    -- Test AND operations
    putStrLn "\nAND operation examples:"
    putStrLn $ "and1 True True: " ++ show (and1 True True)
    putStrLn $ "and2 True False: " ++ show (and2 True False)
    putStrLn $ "and3 False True: " ++ show (and3 False True)
    putStrLn $ "and4 False False: " ++ show (and4 False False)
    putStrLn $ "and5 True True: " ++ show (and5 True True)
    putStrLn $ "and6 True True: " ++ show (and6 True True)
    
    -- Test OR operations
    putStrLn "\nOR operation examples:"
    putStrLn $ "or1 True False: " ++ show (or1 True False)
    putStrLn $ "or2 False True: " ++ show (or2 False True)
    putStrLn $ "or3 False False: " ++ show (or3 False False)
    putStrLn $ "or4 True True: " ++ show (or4 True True)
    putStrLn $ "or5 False True: " ++ show (or5 False True)
    putStrLn $ "or6 True False: " ++ show (or6 True False)
    putStrLn $ "or6 False True (following False || b = b): " ++ show (or6 False True)
    