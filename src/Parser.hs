module Parser 
    ( Expr(..)
    , parseExpr
    )
    where

import Tokenize

type Parser a = [Token] -> Maybe(a, [Token])


symbols =   [ "def"
            , "extern"
            , "if"
            , "then"
            , "else"
            , "in"
            , "for"
            , "binary"
            , "unary"
            , "var"
            ]


data Expr = Var String
            | Val Double
            | Function String [Expr] Expr
            | BinOp Op Expr Expr
            | Call String [Expr]
            | Extern String [Expr]
            deriving (Show, Eq)

{-
Parse an expresion value
-}
parseValue :: Parser Expr
parseValue (Number n:xs)                    = Just (Val n, xs)
--
parseValue (TokenOpen : (TokenClose:xs))    = Just (List [], xs)
-- Launch a parsing instance inside a parenthesis
parseValue (TokenOpen : xs)                 = case parseValue xs of
    Just (Val n, (TokenClose : ys))  -> Just (List [Val n], ys)
    Just (KeyWord n, (TokenClose : ys))  -> Just (List [KeyWord n], ys)
    Just (expr, (TokenClose : ys))  -> Just (expr, ys)
    Just (expr, ys)                 -> Just (List ([expr] ++ recursive), tail rest)
                where
                    (recursive, rest)   = parseExprs [] ys
    Nothing                         -> error "Parse Value return nothing when token open is detected"
-- Launch a recursive to parse the expressions tab
parseValue (TokenOp op :xs)                 = Just (Calcul op recursive, rest)
                where
                    (recursive, rest) = parseExprs [] xs
--
parseValue (Word n:xs)  | n `elem` symbols  =   Just (Symbol n recursive, rest)
                        | n == "'"          =   case parseValue xs of
                            Just (expr, ys) ->  Just (Symbol n [expr], ys)
                            _               ->  error "Parse error for quote'"
                        | otherwise         =   Just (KeyWord n, xs)
                where
                    (recursive, rest) = parseExprs [] xs

-- Error for parsing the value
parseValue x = error ("Token not recognize")

{-
Parse several expressions
-}
parseExprs :: [Expr] -> [Token] -> ([Expr], [Token])
parseExprs list [] = (list, [])
parseExprs list tokens =
    case parseValue tokens of
    -- All the expressions have been parsed
    Just (expr, [])                         -> (list ++ [expr], [])
    -- if the next token is a close parenthesis, the recursive is over
    Just (expr, toks@(TokenClose : xs))     -> (list ++ [expr], toks)
    -- An expression have been parsed
    Just (expr, x)                          -> parseExprs (list ++ [expr]) x
    -- Error during the parsing
    _               -> error "Error during the separation of the expretions"

{-
Launch the expression's parsing instance
-}
parseExpr :: [Token] -> [Expr]
parseExpr [] = []
parseExpr tokens = case parseExprs [] tokens of
    (result, [])    -> result
    _               -> error "bad parsing"