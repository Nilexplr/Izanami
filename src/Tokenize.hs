module Tokenize
    ( stringToToken
    , cleanString
    , reverseList
    , Op(..)
    , drawOp
    , Token(..)
    , ValueType(..)
    )
    where

import Data.Char

data Op = Plus
        | Minus
        | Time
        | Div
        | Mod
        | Inf
        | Sup
        | Eq
        | Not
        | Dif
        | Power
        deriving(Show, Eq)

drawOp :: Op -> String
drawOp Plus = "+"
drawOp Minus = "-"
drawOp Time = "*"
drawOp Div = "div"
drawOp Mod = "mod"
drawOp Inf = "<"
drawOp Sup = ">"
drawOp Eq = "=="
drawOp Not = "!"
drawOp Dif = "!="
drawOp Power = "^"

data ValueType  = ValueDouble Double
                | ValueInt Int
                | ValueString String
                | ValueChar Char
                | ValueBool Bool
                deriving (Show, Eq)
 
data Token = Word String
            | Value ValueType
            | TokenOpen
            | TokenClose
            | TokenOp Op
            | TokenComa
            | TokenSep
            | TokenType
            | TokenQuote
            | TokenSQuote
            | TokenAssign
            deriving(Show, Eq)

cleanString :: String -> String
cleanString [] = []
cleanString s@('#':xs) = cleanString restString
                        where
                            (_, restString) = break counter s
                            counter :: Char -> Bool
                            counter '\n' = True
                            counter _ = False
cleanString (x:xs) = x : cleanString xs

isSpeSpace :: Char -> Bool
isSpeSpace x    | (isAlpha x) = False 
                | (isDigit x) = False
                | x == ';'    = True 
                | x == ','    = True 
                | x == ']'    = True 
                | otherwise   = True 

isFloat :: Char -> Bool
isFloat '.' = True
isFloat x = isDigit x

isEndString :: Char -> Bool
isEndString '"' = False
isEndString x   = True

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

readNumber :: String -> ValueType
readNumber x    | '.' `elem` x  = ValueDouble (read x :: Double)
                | otherwise     = ValueInt (read x :: Int)

stringToToken :: String -> [Token]
stringToToken [] = []
stringToToken s@(x:xs)  | x == '(' = TokenOpen                  : stringToToken xs 
                        | x == ')' = TokenClose                 : stringToToken xs 
                        | x == '+' = TokenOp Plus               : stringToToken xs 
                        | x == '-' = TokenOp Minus              : stringToToken xs 
                        | x == '*' = TokenOp Time               : stringToToken xs 
                        | x == '<' = TokenOp Inf                : stringToToken xs
                        | x == '>' = TokenOp Sup                : stringToToken xs
                        | x == '/' = TokenOp Div                : stringToToken xs
                        | x == '^' = TokenOp Power              : stringToToken xs
                        | x == ';' = TokenComa                  : stringToToken xs
                        | x == '\''= Value (ValueChar (head xs)): stringToToken (tail (tail xs))
                        | x == ',' = TokenSep                   : stringToToken xs
                        | isAlpha x = Word word                 : stringToToken restchar
                        | isSpace x = stringToToken xs
                        | x == '\n' = stringToToken xs
                        | isDigit x = Value (readNumber num)   : stringToToken restnum
                            where
                                (word, restchar)     = break isSpeSpace s
                                (num, restnum)       = break (not . isFloat) s
--
stringToToken ('"':x) = Value (ValueString string)   : stringToToken (tail reststring)
                            where
                                (string, reststring) = break (not . isEndString) x
--
stringToToken ('!':x)   | x == []       = error "Invalid not during Tokenization"
                        | head x == '=' = TokenOp Dif       : (stringToToken $ tail x)
                        | otherwise     = TokenOp Not       : stringToToken x
--
stringToToken ('=':x)   | x == []       = error "Invalid assignation during Tokenization"
                        | head x == '=' = TokenOp Eq      : (stringToToken $ tail x)
                        | otherwise     = TokenAssign  : stringToToken x
stringToToken _ = error "Invalid character"
