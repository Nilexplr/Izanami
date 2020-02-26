module Parser 
    ( Expr(..)
    , TType(..)
    , parseExprs
    )
    where

import Tokenize
import Utils
import Control.Monad.IO.Class
import qualified Data.Text.Lazy.IO as Text
import LLVM.AST.Constant
import LLVM.AST.Float
import LLVM.AST.FloatingPointPredicate hiding (False, True)
import LLVM.AST.Operand
import LLVM.AST.Type as Type
import LLVM.IRBuilder

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


data TType = TDouble Double
            | TInt Int
            | TString String
            | TChar Char
            deriving (Show, Eq)

data Expr = Var String
            | Val TType
            | Function String [Expr] Expr
            | BinOp Op Expr Expr
            | Call String [Expr]
            | Extern String [Expr]
            | List [Expr]
            | Start [Expr]
            deriving (Show, Eq)

{-
Parse an expresion value
-}
parseValue :: Parser Expr
parseValue (Number n:xs)                    = Just ((Val (TInt n)), xs)

-- parseValue (TokenOpen : xs)                 = case parseValue xs of
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
-- parseExprs :: [Expr] -> [Token] -> ([Expr], [Token])
parseExprs :: [Token] -> Expr
parseExprs ((Number n):(TokenOp op:xs))     = BinOp op (Val (TInt n)) $ parseExprs xs
parseExprs ((Number n):(TokenComa:[]))      = Val (TInt n)
parseExprs ((Word name):(TokenOp op:xs))    | op == Assign = BinOp Assign (Var name) $ parseExprs xs
parseExprs _                                = error "Invalid syntax"

-- parseExprs _ -> error "Invalid"

{-
Launch the expression's parsing instance
-}
-- parseExpr :: [Token] -> [Expr]
-- parseExpr [] = []
-- parseExpr tokens = case parseExprs [] tokens of
--     (result, [])    -> result
--     _               -> error "bad parsing"