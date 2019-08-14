module ParseMath(parse) where

import Data.Semigroup
import Data.Maybe
import Data.List.Split

data Operation = Operation Op Priority Handiness deriving (Show)

data Op = Addition | Substraction | Product | Division | LeftP | RightP deriving (Show, Eq)
type Priority = Int
data Handiness = L | R deriving (Show)

-- | Split a string at operators ('+', '-', '*', '/') chars
-- example: -3+4*- 2-5 +8+10/6*2" -> ["-","3","+","4","*","-","2","-","5","+","8","+","10","/","6","*","2"]
splitAtOp :: String -> [String]
splitAtOp [] = []
splitAtOp t@(x:xs) | x `elem` ops = [x] : splitAtOp xs
                   | x `elem` digits = (fst rest) ++ splitAtOp (snd rest)                
                   | otherwise = splitAtOp xs  
                   where rest = go t []
                         go :: String -> String -> ([String], String)
                         go [] buff     = ([buff], [])
                         go h@(x:xss) buff | x `elem` ops    = ([buff], h)
                                           | x `elem` digits = go xss (buff ++ [x])   
                                           | otherwise       = go xss buff

-- clean ops by following these rules = + - a   -> +(-a)
--                                    = - - a   -> +a
--                                    = * - a   -> *(-a)
--                                    = / - a   -> /(-a)
--                                    = o o a   -> o a where o `elem` "+-*/"
--                                    = otherwise -> Invalid
-- returns Nothing if it's malformed
cleanOps :: [String] -> Maybe [String]
cleanOps s = concat <$> (sequence $ f <$> s') -- returns Nothing if any is Nothing
           where s' = coalesce s
                 f :: ([String], String) ->  Maybe [String]
                 f t@([], d) = if t == head s' then Just [d] else Nothing -- fails if there is any ([],"a") which is not the first (in particular
                 f t@(a, d) = validateArr a d                             -- fails if the last char is an op, giving ([],"op"))
                            where validateArr :: [String] -> String -> Maybe [String]
                                  validateArr (x:[]) di = if t == head s' then (if x == "-" then Just ["-"++di] else 
                                                                               (if x == "+" then Just [di] else Nothing)) else 
                                                                               (if x == "-" then Just ["+", "-"++di] else Just [x, di])
                                  validateArr a@(x:y:[]) di | t == head s' = Nothing
                                                            | x == y && x == "-" = Just ["+", di] -- Two -
                                                            | x == y = Just [x, di] -- Two consecutive ops
                                                            | y == "-" && x /= y = Just [x, "-"++di]
                                                            | otherwise = Nothing
                                  validateArr _ _ = Nothing
                                                                                 


-- ["2", "+", "4", "-", "-", "9", "+", "*", "5"] -> [([], "2"), (["+"], "4"), (["-", "-"], "9"), (["+", "*"],"5")]
coalesce :: [String] -> [([String], String)]
coalesce s = g s [] []
           where g :: [String] -> [String] -> [[String]] -> [([String], String)]
                 g [] a z = if a /= [] then toTuple <$> (reverse ((reverse a) : z)) else toTuple <$> (reverse z)
                          where toTuple :: [String] -> ([String], String)
                                toTuple arr | length arr == 1 = ([], head arr)
                                            | length arr > 1 = (init arr, last arr)
                                            | otherwise = ([],[])
                 g (x:xs) a z   | isOp (x !! 0) = g xs (x:a) z
                                | otherwise =  g xs [] ((reverse (x:a)) : z)

-- Parse and interprets a math expression into an integral value, if it's valid
parse :: (Integral a, Read a, Show a) => String -> Maybe a
parse s = if s' == Nothing then Nothing else Just $ g (fromJust s') [] []
        where s' = clean s
              g :: (Integral a, Read a, Show a) => [String] -> [a] -> [String] -> a
              g t@(x:[]) v o                = head $ fst (reduce ((read x):v) o)
              g t@(x:xo:xs) v@[] o@[]       = g xs ((read x):v) (xo:o)
              g t@(x:xo:xs) h@(v:vs) oo@(o:os) | compareOp xo o == False = g xs ((mapOp(o) v (read x) ):vs) (xo:os)
                                               | otherwise = g xs ((read x) : h) (xo : oo)

-- [2, 4, 18] , [+, *, -] -> ([8, 18], [+, -]) -> [10] , [+]
reduce :: Integral a => [a] -> [String] -> ([a], [String])
reduce [] o     = ([],o)
reduce (x:[]) o = ([x],o)
reduce (x:y:[]) (a:[])   = ([(mapOp a) y x], [])
reduce (x:y:xs) (a:b:cs) = reduce ((mapOp a) y x : xs) (b:cs)
                              
-- cleans a string                                               
clean :: String -> Maybe [String]
clean = check . cleanOps . splitAtOp
      where check = (\x -> if x == Nothing || length (fromJust x) < 3 || check' False (fromJust x) == False then Nothing else x)
            check' :: Bool -> [String] -> Bool
            check' _ [] = True
            check' _ (x:[]) = not $ isOpString x
            check' b (x:xs) = if isOpString x == b then (check' (not b) xs) else False




mapOp :: (Integral a) => String -> (a -> a -> a)
mapOp c | c == "+" = (+)
        | c == "-" = (-)
        | c == "*" = (*)
        | c == "/" = div
        | otherwise = const

-- Returns True if the first one has a higher priority
compareOp :: String -> String -> Bool
compareOp op1 op2 | (op1 == "*" || op1 == "/") && (op2 == "+" || op2 == "-") = True
                  | (op1 == "-" || op1 == "+") && (op2 == "*" || op2 == "/") = False -- could be coalesced
                  | otherwise = False

isDigit :: Char -> Bool
isDigit c = elem c digits

isOp :: Char -> Bool
isOp c = elem c ops

isOpString :: String -> Bool
isOpString s = s `elem` (splitOn "" ops)

digits :: String
digits = "0123456789"

ops :: String
ops = "+-*/()"

testString :: String
testString = "4+3*4-3*2*2"

