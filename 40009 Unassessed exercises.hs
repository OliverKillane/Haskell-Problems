-- Imports:
import Data.Char (chr, isSpace )
import Data.List

-- ==============================
-- Haskell Unassessed Exercises
-- ==============================

-- ==============================
-- Section 1: SKIPPED
-- ------------------------------

-- Exercise 1:
-- Part A:  -5
-- Part B:  3 ( - is left associative)
-- Part C: 5
-- Part D: Cannot divide by zero (4 `div` 5 ) == 0
-- Part E:  (^,^^ and ** are right associative)
-- Part F: -17
-- Part G: 12
-- Part H: 16 (precedence of ^/^^/** > */`div` > +/-)
-- Part I: True
-- Part J: 97 (get character vgalue)
-- Part K: 'a'
-- Part L: 'x' 
-- Part M: False
-- Part N: True
-- Part O: 2.0000000000000004 (note this is as sqrt precencece > ^ so it is computing (sqrt 2) ^ 2, better way would bge sqrt (2 ^ 2) which is 2.0)
-- Part P: 4.440892098500626e-16 (very small, a result of imprecise floats)

-- Exercise 2:
-- Part A: let x = 123 in x `mod` 10
-- Part B: let x = 456 in 456 `div` 100
-- Part C: chr ((ord 'a') + 7)

-- Exercise 3:
-- Part A: not $ 2 * 3 == 10
-- Part B: (3 == 4) == True
-- Part C: (if True then 1 else 2) == 3
-- Part D (ord $ if 3 == 4 then 'a' else 'b') +  1
-- Part E: (8 > 2 ^ (if 3 == 4 then 2 else 3)) == False

-- Exercise 4:
-- Part A: (1,2) < (1,4) -> True
-- Part B: ((4, 9), (3, 1)) > ((1, 10), (9, 4)) -> True
-- Part C: (’a’, 5, (3, ’b’)) < (’a’, 5, (3, ’c’)) -> True

-- Exercise 5:
time :: Int -> (Int, Int, Int)
time time = (hours, minutes, seconds)
    where
        hours = time `div` 3600
        minutes = (time `mod` 3600) ` div` 60
        seconds = time `mod` 60

-- Exercise 6:
-- let (r, theta) = (1, pi / 4) in (r * cos(theta), r * sin(theta))

-- Exercise 7:
-- Part A: (3,8,24)
-- Part B: Tuples not the same size (error)
-- Part C: 1
-- Part D: (5, '*')
-- Part E : (True, 1)

-- Exercise 8:
-- (n ‘div‘ 10 * 10 + n ‘div‘ 10) * 100 + (n ‘mod‘ 10 * 10 + n ‘mod‘ 10) for n
-- (11 * (n `div` 10)) * 100 + (11 * (n `mod` 10))
-- takes 10s unit (X) and units (Y) makes XXYY

-- Exercise 9:
-- Part A: filter (>'j') "As you like it"
-- Part B: filter(\x -> x `mod` 10 == 3) [1,13..76*13]
-- Part C: [(x, y, x*y) | x <- [2..12], y <- [2..12]]
-- Part D: [(card, suit) | card <- "JQKA", suit <- "CDHS"]
-- Part E: [(m, n) | n <- [1..100], m <- [n..100], (m + n) == (m - n) ^ 2 ]

-- ==============================
-- Section 2: FUNCTIONS
-- ------------------------------

-- Exercise 1:
addDigit :: Integer -> Integer-> Integer -- Use of integer to avoid possibility of overflow (yes I knikw it said to ignore that)
addDigit longInt addDigit
    | addDigit > 9 = error "Can only append a single digit."
    | addDigit < 0 = error "Must be a positive digit."
    | otherwise = 10 * longInt + addDigit

-- Exercise 2:
celtoFaren :: Float -> Float
celtoFaren x = (x - 32) * (5/9)


-- Exercise 3:

type Vertex = (Float, Float)

distance :: Vertex -> Vertex -> Float
distance x y = d 
    where
        dx = fst x - fst y
        dy = snd x - snd y
        d = sqrt $ dx^2 + dy^2


-- Exercise 4:
triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea x y z = sqrt (s*(s-a)*(s-b)*(s-c))
    where
        a = distance x y
        b = distance x z
        c = distance z y
        s = (a + b + c) / 2
    
-- Exercise 5.
-- Recursion used so that if a number is found to not be prime before reaching its root, then you can return.
isPrime :: Int -> Bool
isPrime x
    | x <= 1 = False
    | otherwise = checkPrime at
    where
        at = 2
        max =  round (sqrt (fromIntegral x))
        checkPrime at
            | x `mod` at == 0 = False
            | at == max = True
            | otherwise = checkPrime (at + 1)

-- Alternatively (slower by still cool)
isPrime' :: Int -> Bool
isPrime' x 
    | x <= 1 = False
    | otherwise = foldr (&&) True [x `mod` y /= 0 | y <- [2..(round (sqrt (fromIntegral x)))]]

-- Exercise 6:
fact :: Int -> Int
fact n 
    | n == 0 = 1
    | otherwise = n * fact (n - 1)

-- alternatively without guards
fact' :: Int -> Int
fact' 0 = 1
fact' n = n * fact (n - 1)

-- Exercise 7:
perm :: Int -> Int -> Int
perm n r
    | n < r = error "n must be larger than or equal to r"
    | r < 0 = error "error cannot have negative numbers" -- This also catched N < 0 as n >= r so if n is negative, r must be also
    | otherwise = permGen n
        where
            permGen x
                | x == n - r + 1 = n - r + 1
                | otherwise = x * permGen( x - 1)

-- NOTE: Alternative below does not count as it does not use recursion.
perm' :: Int -> Int -> Int
perm' n r = foldr (*) 1 [(n - r + 1)..n]

-- Exercise 8:
nCr :: Float -> Float -> Float -- Have to use float because of use of / so redo this later.
nCr n r
    | r == 1 = n
    | otherwise = (n / r) * nCr (n - 1) (r - 1)

-- Exercise 9:
remainder :: Int -> Int -> Int
remainder x y
    | x < 0 || y < 0 = error "No negatives please."
    | x < y = x
    | otherwise = remainder (x - y) y

-- Exercise 10:
quotient :: Int -> Int -> Int
quotient x y 
    | y == 0 = error "Zero division"
    | x < y = 0
    | otherwise = 1 + quotient (x - y) y

-- Exercise 11:
dentobin :: Int -> Int
dentobin x
    | x < 2 = x
    | otherwise = x `mod` 2 + 10 * dentobin (x `div` 2) 

denton :: Int -> Int -> Int
denton x n
    | x < n = x
    | otherwise = x `mod` n + 10 * dentobin (x `div` n) 

-- Exercise 12:
-- Part A:
add :: Int -> Int -> Int
add x y 
    | y == 0 = x
    | otherwise = add (succ x) (pred y)

-- Part B: (skipped, how to do recurively??)
larger :: Int -> Int -> Int
larger x y
    | x > y = x
    | otherwise = y

-- Exercise 13:
chop :: Int -> (Int, Int)
chop x = (a, b)
    where
        a = choprecur x
        b = x - a * 10
        choprecur x
            | x > 10 = 1 + choprecur (x - 10)
            | otherwise = 0

-- Exercise 14:
concatenate :: Int -> Int -> Int
concatenate x y = x * (10 ^ (concatrecur y)) + y
    where
        concatrecur z
            | z < 10 = 1
            | otherwise = 1 + concatrecur (z `div` 10)

-- Exercise 15:
fibonacci :: Int -> Int
fibonacci n 
    | n > 2 = fibrecur 1 2 (n - 3)
    | n <= 0 = error "no negative values please"
    | otherwise = n
    where
        fibrecur x y n
            | n <= 0 = x + y
            | otherwise = fibrecur y (x + y) (n -1)

-- Exercise 16:
golden :: Float -> Float
golden e = goldenrecur 1 2 3
    where
        goldenrecur a b c
            | abs(c/b - b/a) < e = c/b
            | otherwise = goldenrecur b c (b + c)

-- ==============================
-- Section 3: LISTS
-- ------------------------------

-- Exercise 1:
-- Part A (Not correctly typed, as "H" is a string/char list, and infix operator : is a -> [a] -> [a])
-- IMPROVED: 'H' : ['a','s','k','e','l','l'] -> "Haskell"
-- Part B (Correctly typed) -> "ongoing"
-- Part C: (Correctly typed) -> "Lugworm"
-- Part D: (Correctly typed) -> [] or ""
-- Part E: (Correctly typed) -> 1 (technically true, however the user really wanted 6, but created list ["let xs"] which is a 2D array of length 1, 6)
-- IMPROVED: let xs = "let xs" in length xs -> 6
-- Part F: (Incorrectly typed, as : is a -> [a] -> [a]  and two lists are provided)
-- Part G: (Correctly typed) -> "gasket" (techically works, but much like in Part E, its a 2D array, really they want head "gasket")
-- IMPROVED: head "gasket" -> "g"
-- Part H: (Correctly typed) -> [('1', 1), ('2', 2)]
-- Part I: (Incorrectly typed, as tail works on [a] -> [a] not a tuple)
-- IMPROVED: tail [((1,1), (2,2), (3,3)]
-- Part J: (Bady incorrectly typed, as the tuples cannot be contained in a list)
-- IMPROVED: head [1,2,3] /= head [2,3,1] -> True
-- Part K: (Correctly typed) -> 2
-- Part L: (Correctly typed) -> False (checking if the second element of the list is null)
-- Part M: (Incorrectly typed, technically works, but not what I think the programmer wants) -> [(5, (True,False,True))]
-- IMPROVED: zip [5, 3, length []] [True, False, True] -> [(5, True), (3, False), (0, True)]
-- Part N: (Correctly typed) -> ("bad", "dog")
-- Part O: (Correctly typed) -> True
-- Part P: (Correctly typed) -> 9
-- Part Q: (Correctly typed) -> ('a', 2) (this is as the first item in the tuple is small (the ascii character code is lower than b or c))
-- Part R: (Inorrectly typed, this is as due to operator precedence, maximum is worked on first, and a parameter is left missing)
-- IMPROVEMENT: maximum $ zip "abc" [1,2,3] -> ('c' 3)
-- Part S: (Incorrectly typed, technically works, but due to the extra [] around tail it is concatenating a single element list) -> ["not", "with", "standing"]
-- IMPROVEMENT: concat $ tail ["is", "not", "with", "standing"] -> "notwithstanding"
-- Part T: (Incorrectly typed, as head returns a single, as head wants a list, and as n is undefined)
-- Part U: (Correctly typed) -> ""
-- Part V: (Correctly typed) -> True
-- Part W: (Incorrectly typed, as the second argument for zipwith must be a list as the operator : works on char : List[Char])
-- IMPROVED: zipWith (:) "zip" ["w","i","t","h"]
-- Part X: (Correctly typed) -> 1
-- Part Y: (Correctly typed) -> wrong, cannot find reason
-- Part Z: (Correctly typed, thougb odd that they are zipping with only true) -> [True]

-- Exercise 2:
precedes :: Ord a => a -> a -> Bool
precedes x y = x <= y

-- Exercise 3:
pos :: Int -> [Int] -> Int
pos x xs
    | x <= 1 = head xs
    | otherwise = pos (x - 1) (tail xs)

--alternatively
pos' :: Int -> [a] -> a
pos' x xs = xs!!(x - 1)

-- Exercise 4:
twosame :: Eq a => [a] -> Bool
twosame xs = foldr (||) False (map (checker) [1..(length xs - 1)])
    where
        checker x = elem (xs!!x) ((take (x) xs) ++ (drop (x + 1) xs))

-- Exercise 5:
rev :: [a] -> [a]
rev (x:xs)
    | length xs == 0 = [x]
    | otherwise = rev xs ++ [x]

-- for list length n, there are n + 1 recursions (for all elements and for the base case), so O(n)

-- Exercise 6:
substring :: String -> String -> Bool
substring x y = elem x  (gensubs y)
    where
        takelen = length x - 1
        gensubs (x:xs)
            | length xs <= takelen = []
            | otherwise = [[x] ++ take takelen xs] ++ gensubs xs

-- Exercise 7:
transpose :: String -> String -> String -> String

-- y is the input string, x1 contains initial positions, x2 contains the transformation
-- List comprehension goes through final positions (x2), gets the index of that character in the original positions (x1),
-- then uses that same index to get the next character in the tranformed string.
transpose y x1 x2 = [y!!(findchar z x1 ) | z <- x2]

    -- Finchar continues 'peeling' elements of one by one until it hits an error or gets the character being searched for, 
    -- uses original length and current check length to work out index.
    where findchar j (x:xs)
            | length (x:xs) == 0 = error "issue with parameter list contents"
            | x == j = length x1 - (length xs + 1)
            | otherwise = findchar j xs


--Exercise 8:
removeWhitespace :: String -> String
removeWhitespace (x:xs)
    | isSpace x = removeWhitespace xs
    | otherwise = x:xs

-- Exercise 9:
nextWord :: String -> (String, String)
nextWord s = splitAt ( splitnum y) y
    where
        y = removeWhitespace s
        splitnum (x:xs)
            | length (x:xs) == 0 = 0
            | length xs == 0 = 0
            | isSpace x =  0
            | otherwise = 1 + (splitnum xs)

-- Exercise 10:
splitUp :: String -> [String]
splitUp x
    | null x = []
    | otherwise = [word] ++ (splitUp therest)
        where
            (word, therest) = nextWord x

-- Exercise 11:
primeFactors :: Int -> [Int]
primeFactors x
    | x == 1 = []
    | otherwise = z:(primeFactors  ( x `div` z))
        where
            z = nextFactor 2 x
            nextFactor check orig 
                | orig `mod` check == 0 = check
                | otherwise = nextFactor (check + 1) orig
            
-- Exercise 12:
highestcomfact :: Int -> Int -> Int
highestcomfact a b = foldr (*) 1 (unionlists x y)
    where
        x = primeFactors a
        y = primeFactors b

unionlists :: [Int] -> [Int] -> [Int]
unionlists _ [] = []
unionlists [] _ = []
unionlists (a:as) (b:bs)
    | length as == 0 || length bs == 0 = if a == b then [a] else []
    | a == b             = a:(unionlists as bs)
    | a > b              = unionlists (a:as) bs
    | a < b              = unionlists as (b:bs)
    | otherwise          = error "shouldn't be here"

-- Exercise 13:
lowcm :: Int -> Int -> Int
lowcm x y = foldr (*) 1 $ (xs \\ common) ++ common ++ (ys \\ common)
    where
        xs = primeFactors x
        ys = primeFactors y
        common = unionlists xs ys



-- ==============================
-- Section 3.2: LIST COMPREHENSIONS
-- ------------------------------

-- Exercise 1:
-- The issue is it is not checking for condition when going through t
findAll :: Eq a => a -> [(a,a)] -> [a]
findAll x t = [y | (z, y) <- t, x == z]

-- Exercise 2:
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x t = [z | z <- t, x /= fst z]

-- Exercise 3:
quicksort :: Ord a => [a] -> [a]
quicksort xs
    | length xs <= 1 = xs
    | otherwise = (quicksort $ filter (<pivot) xs) ++ [pivot] ++ (quicksort $ filter (>pivot) xs)
        where
            pivot = xs !! (length xs `div` 2)

-- Exercise 4:
allSplits :: Ord a => [a] -> [([a], [a])]
allSplits xs = map (flip splitAt xs) [1..(length xs - 1)]


-- Exercise 5:
prefixes :: Ord a => [a] -> [[a]]
prefixes xs = (map fst $ allSplits xs) ++ [xs]


-- Exercise 6:
substrings :: Ord a => [a] -> [[a]]
substrings [] = []
substrings all@(x:xs) = prefixes all ++ substrings xs

-- Exercise 7:
-- Using Heap's Algorithm
perms :: [a] -> [[a]]
perms [x] = [[x]]
perms (x:xs) = concat [[take y pem ++ [x] ++ drop y pem | y <- [0..length pem]] | pem <- perms xs]

-- Exercise 8:
routes :: Int -> Int -> [(Int,Int)] -> [[Int]]
routes start end routelist
    | start == end = [[end]]
    | otherwise = map (start:) $ concat [(routes inter end routelist) | (start', inter) <- routelist, start' == start]

cyclicRoutes :: Int -> Int -> [(Int,Int)] -> [[Int]]
cyclicRoutes start end routelist = cyclicRoutesHelper [] start end
    where 
        cyclicRoutesHelper :: [Int] -> Int -> Int -> [[Int]]
        cyclicRoutesHelper prev start end
            | start == end = [[end]]
            | elem start prev = []
            | otherwise = map (start:) $ concat [(cyclicRoutesHelper (start:prev) inter end) | (start', inter) <- routelist, start' == start]

-- ==============================
-- Section 4: Higher order functions
-- ------------------------------

-- Exercise 1:
-- Part A:
depunctuate :: String -> String
depunctuate = filter (\x -> not $ elem x ".,:")

-- Part B:
makeString :: [Int] -> String
makeString = map (chr)

-- Part C:
enpower :: [Int] -> Int
enpower = foldr (^) 1

-- Part D:
revAll :: [[a]] -> [a]
revAll = concatMap (reverse)

-- Part E:
rev' :: [a] -> [a]
rev' = foldl (flip (:)) [] 

-- Part F:
dezip :: [(a,b)] -> ([a],[b])
dezip zipped = (map (\(x,_) -> x) zipped, map (\(_,y) -> y) zipped)

-- Exercise 2:
allSame :: [Int] -> Bool
allSame all@(l:list) = foldr (&&) True $ zipWith (==) (list ++ [l]) all

-- Exercise 3:
-- Part A:
factorials :: [Integer]
factorials = scanl (*) 1 [1..]

-- Part B:
-- Changed to first n elements for fun
eBy :: Int -> Double
eBy n = foldr (+) 0 $ map (\x -> 1 / fromIntegral(factorials!!x)  ) [0..n]

-- Part C:
-- It computes the bibonacci numbers! as thee a a recursion in the let statement.
--let xs = 1 : scanl (+) 1 xs in xs

-- Exercise 4:


-- For testing
main::IO()
main = do
    print (eBy 70)
    


-- For running exercises:
-- Section 1:
--      Exercise 1	
--          Part A  ✓ 
--          Part B  ✓ 
--          Part C  ✓ 
--          Part D  ✓ 
--          Part E  ✓ 
--          Part F  ✓ 
--          Part G  ✓ 
--          Part H  ✓ 
--          Part I  ✓ 
--          Part J  ✓ 
--          Part K  ✓ 
--          Part L  ✓ 
--          Part M  ✓ 
--          Part N  ✓ 
--          Part O  ✓ 
--          Part P  ✓ 
--      Exercise 2	
--          Part A  ✓ 
--          Part B  ✓ 
--          Part C  ✓ 
--      Exercise 3	
--          Part A  ✓ 
--          Part B  ✓ 
--          Part C  ✓ 
--          Part D  ✓ 
--          Part E  ✓ 
--      Exercise 4	
--          Part A  ✓ 
--          Part B  ✓ 
--          Part C  ✓ 
--      Exercise 5	✓
--      Exercise 6	✓
--      Exercise 7	
--          Part A  ✓ 
--          Part B  ✓ 
--          Part C  ✓ 
--          Part D  ✓ 
--          Part E  ✓ 
--      Exercise 8	✓
--      Exercise 9	
--          Part A  ✓ 
--          Part B  ✓ 
--          Part C  ✓ 
--          Part D  ✓ 
--          Part E  ✓ 
-- Section 2:
--      Exercise 1	✓ 
--      Exercise 2	✓
--      Exercise 3	✓
--      Exercise 4	✓
--      Exercise 5	✓
--      Exercise 6	✓
--      Exercise 7	?
--      Exercise 8	?
--      Exercise 9	✓
--      Exercise 10	✓
--      Exercise 11	✓
--      Exercise 12
--          Part A  ✓
--          Part B  
--      Exercise 13	
--      Exercise 14	
--      Exercise 15	
--      Exercise 16	
-- Section 3:
--      Exercise 1
--          Part A  ✓ 
--          Part B  ✓ 
--          Part C  ✓ 
--          Part D  ✓ 
--          Part E  ✓ 
--          Part F  ✓ 
--          Part G  ✓ 
--          Part H  ✓ 
--          Part I  ✓ 
--          Part J  ✓ 
--          Part K  ✓ 
--          Part L  ✓ 
--          Part M  ✓ 
--          Part N  ✓ 
--          Part O  ✓ 
--          Part P  ✓ 
--          Part Q  ✓ 
--          Part R  ✓ 
--          Part S  ✓ 
--          Part T  ✓ 
--          Part U  ✓ 
--          Part V  ✓ 
--          Part W  ✓ 
--          Part X  ✓ 
--          Part Y  ✓ 
--          Part Z  ✓ 
--      Exercise 2  ✓
--      Exercise 3  ✓
--      Exercise 4  ✓
--      Exercise 5  ✓
--      Exercise 6  ✓
--      Exercise 7  ✓
--      Exercise 9  ✓
--      Exercise 10 ✓
--      Exercise 11 ✓
--      Exercise 12 
--      Exercise 13 
-- Section 3.2:
--      Exercise 1  ✓
--      Exercise 2  ✓
--      Exercise 3  ✓
--      Exercise 4  ✓
--      Exercise 5  ✓
--      Exercise 6  ✓
--      Exercise 7  ✓
--      Exercise 8  ✓