module Parser 
    ( Expr(..)
    , ValueType(..)
    , createAst
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
           
            
data Expr   = Var       String                          ExprType
            | Val       ValueType                       ExprType
            | UnaryOp   Op      Expr                    ExprType
            | Call      String  [Expr]                  ExprType
            | Extern    String  [Expr]                  ExprType
            | Function  String  [Expr]  Expr            ExprType
            | BinOp     Op      Expr    Expr            ExprType
            | If        Expr    Expr    (Maybe Expr)    ExprType
            | For       String  Expr    Expr    Expr    ExprType
            | List      [Expr]                          ExprType
            | Ast       [Expr]                          ExprType
            deriving (Show, Eq)

setTypeExpr :: Expr -> ExprType -> Expr
setTypeExpr (Var x _) typage = Var x typage  
-- setTypeExpr (Val x _) typage = Val x typage  
-- setTypeExpr (Var x _) typage = Var x typage  

typeValueToExpr :: ValueType -> ExprType
typeValueToExpr (ValueDouble x) = ExprDouble
typeValueToExpr (ValueString x) = ExprString
typeValueToExpr (ValueChar   x) = ExprChar
typeValueToExpr (ValueInt    x) = ExprInt


{-
Parse an expresion value
-}
parseValue :: Parser Expr
parseValue (Value x:xs)         = Just ((Val x (typeValueToExpr x)), xs)
parseValue (TokenOpen : xs)     = case parseExpr xs of
    Just (expr, (TokenClose : ys))  -> Just (expr, ys)
    Nothing                         -> error "Parse Value return nothing when token open is detected"

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

-- parseUnOp :: Op -> Parser Expr
-- parseUnOp op tokens = case parseExpr tokens of
--     Just (x, toks)      -> Just (BinOp op previousExpr x None, toks)
--     _                   -> Nothing

parseBinOp :: Expr -> Op -> Parser Expr
parseBinOp previousExpr op tokens = case parseExpr tokens of
    Just (x, toks)      -> Just (BinOp op previousExpr x None, toks)  
    _                   -> Nothing

parseExpr :: Parser Expr
parseExpr token = case parseValue token of
    Just (x , (TokenOp op:xs))      -> parseBinOp x op xs
    Just (x , toks)-> Just (x, toks)

{-
Parse several expressions
-}
parseExprs :: [Expr] -> Parser [Expr]
parseExprs list [] = Just (list, [])
parseExprs list tokens =
    case parseExpr tokens of
    -- All the expressions have been parsed
    Just (expr, toks@(TokenComa : []))      -> Just (list ++ [expr], [])
    -- if the next token is a close parenthesis, the recursive is over
    Just (expr, toks@(TokenComa : xs))      -> parseExprs (list ++ [expr]) xs
    -- Error during the parsing    
    _                                       -> Nothing


{-
Launch the expression's parsing instance
-}
createAst :: [Token] -> Expr
createAst [] = Ast [] None
createAst tokens = case parseExprs [] tokens of
    Just (result, [])   -> Ast result None
    _                   -> error "bad parsing"