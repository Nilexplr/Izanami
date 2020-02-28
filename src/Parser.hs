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

data ExprType   = ExprDouble 
                | ExprInt 
                | ExprString 
                | ExprChar
                | ExprBool
                | None
                deriving (Eq, Show, Typeable)
           
            
data Expr   = Var       String                          ExprType
            | Val       ValueType                       ExprType
            | UnaryOp   Op      Expr                    ExprType
            | Assign    Expr    Expr                    ExprType
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
setTypeExpr (Var       x        _)  typage = Var       x        typage  
setTypeExpr (Val       x        _)  typage = Val       x        typage 
setTypeExpr (UnaryOp   op x     _)  typage = UnaryOp   op x     typage
setTypeExpr (Call      x y      _)  typage = Call      x y      typage 
setTypeExpr (Extern    x y      _)  typage = Extern    x y      typage 
setTypeExpr (Function  x y z    _)  typage = Function  x y z    typage
setTypeExpr (BinOp     x y z    _)  typage = BinOp     x y z    typage
setTypeExpr (If        x y z    _)  typage = If        x y z    typage
setTypeExpr (For       x y z a  _)  typage = For       x y z a  typage
setTypeExpr (List      x        _)  typage = List      x        typage 

typeValueToExpr :: ValueType -> ExprType
typeValueToExpr (ValueDouble x) = ExprDouble
typeValueToExpr (ValueString x) = ExprString
typeValueToExpr (ValueChar   x) = ExprChar
typeValueToExpr (ValueInt    x) = ExprInt

getTypefromExpr :: Expr -> ExprType
getTypefromExpr (Var       _        exprtype)   = exprtype 
getTypefromExpr (Val       _        exprtype)   = exprtype 
getTypefromExpr (UnaryOp   Not _    exprtype)   = ExprBool 
getTypefromExpr (UnaryOp   op _     exprtype)   = exprtype
getTypefromExpr (Call      _ _      exprtype)   = exprtype 
getTypefromExpr (Extern    _ _      exprtype)   = exprtype 
getTypefromExpr (Function  _  _ _   exprtype)   = exprtype
getTypefromExpr (BinOp     _  _ _   exprtype)   = exprtype
getTypefromExpr (If        _ _ _    exprtype)   = exprtype
getTypefromExpr (For       _ _ _ _  exprtype)   = exprtype
getTypefromExpr (List      _        exprtype)   = exprtype

parseUnOp :: Op -> Parser Expr
parseUnOp Not tokens = case parseExpr tokens of
    Just (x, toks)      -> Just (UnaryOp Not x ExprBool, toks)
    _                   -> Nothing
parseUnOp Plus tokens = case parseExpr tokens of
    Just (x, toks)      -> Just (UnaryOp Plus x (getTypefromExpr x), toks)
    _                   -> Nothing
parseUnOp Minus tokens = case parseExpr tokens of
    Just (x, toks)      -> Just (UnaryOp Minus x (getTypefromExpr x), toks)
    _                   -> Nothing
parseUnOp _ _          = error "Bad Unop symbol"

-- parseFor :: Parser Expr
-- parseIf tokens = Nothing

parseIf :: Parser Expr
parseIf tokens = case parseExpr tokens of 
    Just (x, Word "then":xs)    ->  case parseExpr xs of
        Just (y, Word "else":ys)    -> case parseExpr ys of
            Just (z, zs)                -> Just (If x y (Just z) (getTypefromExpr y), zs)
        Just (y, ys)                -> Just (If x y Nothing (getTypefromExpr y), ys)
    _                           -> error "Bad If expressions"

-- parseWhile :: Parser Expr
-- parseWhile tokens = Nothing

parseAssign :: Expr -> Parser Expr
parseAssign name tokens = case parseExpr tokens of
    Just (x, tokens)    -> Just (Assign (setTypeExpr name (getTypefromExpr x)) x (getTypefromExpr x), tokens)

{-
Parse an expresion value
-}
parseValue :: Parser Expr
parseValue (Value x:xs)         = Just ((Val x (typeValueToExpr x)), xs)
parseValue (TokenOpen : xs)     = case parseExpr xs of
    Just (expr, (TokenClose : ys))  -> Just (expr, ys)
    Nothing                         -> error "Parse Value return nothing when token open is detected"
parseValue (TokenOp op: xs)     = parseUnOp op xs
--
parseValue (Word n:xs)  -- | n == "for" = parseFor xs
                        | n == "if"  = parseIf xs
                        | otherwise  = Just ((Var n None), xs)
            --
-- Error for parsing the value
parseValue x = error ("Token not recognize")


parseBinOp :: Expr -> Op -> Parser Expr
parseBinOp previousExpr op tokens = case parseExpr tokens of
    Just (x, toks)      -> Just (BinOp op previousExpr x None, toks)  
    _                   -> Nothing

parseExpr :: Parser Expr
parseExpr token = case parseValue token of
    Just (x , (TokenOp op:xs))      -> parseBinOp x op xs
    Just (x , (TokenAssign:xs))     -> parseAssign x xs
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
    -- Expr still left to parse
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