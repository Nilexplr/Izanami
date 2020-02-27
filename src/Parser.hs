module Parser 
    ( Expr(..)
    , ValueType(..)
    , parseExprs
    , ExprType(..)
    )
    where

import Tokenize
import Data.Data

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

data ExprType   = ExprDouble 
                | ExprInt 
                | ExprString 
                | ExprChar
                | None
                deriving (Eq, Show, Typeable)
           
            
data Expr   = Var       String                  ExprType
            | Val       ValueType               ExprType
            | UnaryOp   Op      Expr            ExprType
            | Call      String  [Expr]          ExprType
            | Extern    String  [Expr]          ExprType
            | Function  String  [Expr]  Expr    ExprType
            | BinOp     Op      Expr    Expr    ExprType
            | If        Expr    Expr    Expr    ExprType
            | For       Expr    Expr    Expr    ExprType
            | List      [Expr]                  ExprType
            | AST       [Expr]                  ExprType
            deriving (Show, Eq)

type TypedExpr = (ExprType , Expr)

typeValueToExpr :: ValueType -> ExprType
typeValueToExpr ValueDouble x = ExprDouble
typeValueToExpr ValueString x = ExprString
typeValueToExpr ValueChar   x = ExprChar
typeValueToExpr ValueInt    x = ExprInt

{-
Parse an expresion value
-}
parseValue :: Parser Expr
parseValue (Value x:xs)         = Just ((Val x (typeValuetoExpr x)), xs)
parseValue (TokenOpen : xs)     = case parseValue xs of
--     Just (Val n, (TokenClose : ys))  -> Just (List [Val n], ys)
--     Just (KeyWord n, (TokenClose : ys))  -> Just (List [KeyWord n], ys)
--     Just (expr, (TokenClose : ys))  -> Just (expr, ys)
--     Just (expr, ys)                 -> Just (List ([expr] ++ recursive), tail rest)
--                 where
--                     (recursive, rest)   = parseExprs [] ys
--     Nothing                         -> error "Parse Value return nothing when token open is detected"

-- parseValue (Word n:xs)  | n `elem` symbols  =   Just (Symbol n recursive, rest)
--                         | n == "'"          =   case parseValue xs of
--                             Just (expr, ys) ->  Just (Symbol n [expr], ys)
--                             _               ->  error "Parse error for quote'"
--                         | otherwise         =   Just (KeyWord n, xs)
--                 where
--                     (recursive, rest) = parseExprs [] xs

parseValue (TokenOpen : (TokenClose:xs))    = error "Invalid syntax"
-- Error for parsing the value
parseValue x = error ("Token not recognize")

{-
Parse several expressions
-}
parseExprs :: [Expr] -> Parser [Expr]

parseExprs _                                = error "Invalid syntax"


{-
Launch the expression's parsing instance
-}
parseExpr :: [Token] -> Expr
parseExpr [] = AST [] None
parseExpr tokens = case parseExprs [] tokens of
    Just (result, [])   -> AST result None
    _                   -> error "bad parsing"