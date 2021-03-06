module Parser 
    ( Expr(..)
    , ValueType(..)
    , createAst
    , ExprType(..)
    , getTypefromExpr
    )
    where


import Tokenize
import Data.Data
import Data.List

type Parser a = [Token] -> Maybe(a, [Token])

data ExprType   = ExprDouble 
                | ExprInt 
                | ExprString 
                | ExprChar
                | ExprBool
                | None
                deriving (Eq, Show, Typeable)      
            
data Expr   = Var       String                                  ExprType --
            | Val       ValueType                               ExprType --
            | UnaryOp   Op      Expr                            ExprType --
            | Assign    Expr    Expr                            ExprType --
            | While     Expr    Expr                            ExprType --
            | Call      String  [Expr]                          ExprType --
            | Extern    String  [Expr]                          ExprType --
            | Function  String  [Expr]  Expr                    ExprType --
            | BinOp     Op      Expr    Expr                    ExprType -- ~FIX EVAL~
            | If        Expr    Expr    (Maybe Expr)            ExprType --
            | For       Expr    Expr    (Maybe Expr)    Expr    ExprType --
            | List      Expr    Expr                            ExprType --
            | Ast       [Expr]                                  ExprType --
            deriving (Show, Eq)

getTypeFromTypeName :: Expr -> ExprType
getTypeFromTypeName (Var name _)    | name == "int"     = ExprInt
                                    | name == "double"  = ExprDouble
                                    | name == "void"    = None
                                    | name == "bool"    = ExprBool
                                    | name == "char"    = ExprChar
                                    | name == "string"  = ExprString
                                    | otherwise         = error "Bad type declared"
--
getTypeFromTypeName   _                                 = error "Type should be a name"

setTypeExpr :: Expr -> ExprType -> Expr
setTypeExpr (Var       x        _)  typage = Var       x        typage  
setTypeExpr (Val       x        _)  typage = Val       x        typage 
setTypeExpr (UnaryOp   op x     _)  typage = UnaryOp   op x     typage
setTypeExpr (Call      x y      _)  typage = Call      x y      typage 
setTypeExpr (Assign    x y      _)  typage = Assign    x y      typage 
setTypeExpr (While     x y      _)  typage = While     x y      typage 
setTypeExpr (Extern    x y      _)  typage = Extern    x y      typage 
setTypeExpr (Function  x y z    _)  typage = Function  x y z    typage
setTypeExpr (BinOp     x y z    _)  typage = BinOp     x y z    typage
setTypeExpr (If        x y z    _)  typage = If        x y z    typage
setTypeExpr (For       x y z a  _)  typage = For       x y z a  typage
setTypeExpr (List      x y      _)  typage = List      x y      typage 

typeValueToExpr :: ValueType -> ExprType
typeValueToExpr (ValueDouble x) = ExprDouble
typeValueToExpr (ValueString x) = ExprString
typeValueToExpr (ValueChar   x) = ExprChar
typeValueToExpr (ValueInt    x) = ExprInt

getTypefromExpr :: Expr -> ExprType
getTypefromExpr (Var       _        exprtype)   = exprtype 
getTypefromExpr (Val       _        exprtype)   = exprtype 
getTypefromExpr (UnaryOp   Not _    exprtype)   = exprtype 
getTypefromExpr (UnaryOp   op _     exprtype)   = exprtype
getTypefromExpr (Call      _ _      exprtype)   = exprtype 
getTypefromExpr (While     _ _      exprtype)   = exprtype 
getTypefromExpr (Assign    _ _      exprtype)   = exprtype 
getTypefromExpr (Extern    _ _      exprtype)   = exprtype 
getTypefromExpr (Function  _  _ _   exprtype)   = exprtype
getTypefromExpr (BinOp     _  _ _   exprtype)   = exprtype
getTypefromExpr (If        _ _ _    exprtype)   = exprtype
getTypefromExpr (For       _ _ _ _  exprtype)   = exprtype
getTypefromExpr (List      _ _      exprtype)   = exprtype

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

parseName :: Parser Expr
parseName (Word n:xs)  = Just(Var n None, xs)
parseName _             = Nothing

parseFor :: Parser Expr
parseFor tokens = case parseExpr tokens of
    Just (x, TokenSep:xs)   -> case parseExpr xs of
        Just (y, TokenSep:ys)   -> case parseExpr ys of
            Just (z, Word "in":zs)  -> case parseExpr zs of
                Just (final, restoks) -> Just (For x y  (Just z) final (getTypefromExpr x), restoks)
                _                     -> error "Bad in parsing for expr"
            _                     -> error "Bad third sep parsing for expr"
        Just (z, Word "in":zs)  -> case parseExpr zs of
            Just (final, restoks) -> Just (For x z Nothing final (getTypefromExpr x), restoks)
        _                     -> error "Bad second sep parsing for expr"
    _                     -> error "Bad first sep parsing for expr"

parseIf :: Parser Expr
parseIf tokens = case parseExpr tokens of 
    Just (x, Word "then":xs)    ->  case parseExpr xs of
        Just (y, Word "else":ys)    -> case parseExpr ys of
            Just (z, zs)                -> Just (If x y (Just z) (getTypefromExpr y), zs)
        Just (y, ys)                -> Just (If x y Nothing (getTypefromExpr y), ys)
    _                           -> error "Bad If expressions"

parseWhile :: Parser Expr
parseWhile tokens = case parseExpr tokens of
    Just (x, Word "do":xs) -> case parseExpr xs of
        Just (y, yz)    -> Just (While x y (getTypefromExpr x), yz)

parseAssign :: Expr -> Parser Expr
parseAssign name tokens = case parseExpr tokens of
    Just (x, tokens)    -> Just (Assign (setTypeExpr name (getTypefromExpr x)) x (getTypefromExpr x), tokens)

parseAPrototype :: Parser Expr
parseAPrototype tok = case parseExpr tok of
    Just ((Var n _), TokenType:xs) -> case parseValue xs of
        Just (y, ys) -> Just (Var n (getTypeFromTypeName y), ys)
    Just (x, xs) -> Just (x, xs)

parsePrototype :: [Expr] ->  Parser [Expr]
parsePrototype tab toks@(TokenOpen:(TokenClose:ys)) = Just ([], ys)
parsePrototype tab (TokenOpen:xs)   =  case parseAPrototype xs of
    Just (expr, toks@(TokenClose: ys))      -> Just (tab ++ [expr], ys)
    Just (expr, toks)                       -> parsePrototype (tab ++ [expr]) toks
    _                                       -> Nothing
parsePrototype tab tokens           = case parseAPrototype tokens of
    Just (expr, toks@(TokenClose: ys))      -> Just (tab ++ [expr], ys)
    Just (expr, toks)                       -> parsePrototype (tab ++ [expr]) toks
    _                                       -> Nothing  

parseArgs :: [Expr] ->  Parser [Expr]
parseArgs tab toks@(TokenOpen:(TokenClose:ys)) = Just (tab, ys)
parseArgs tab toks@(TokenClose:ys) = Just (tab, ys)
parseArgs tab (TokenOpen:xs)   =  case parseAPrototype xs of
    Just (expr, toks@(TokenClose: ys))      -> Just (tab ++ [expr], ys)
    Just (expr, toks@(TokenSep: ys))        -> parseArgs (tab ++ [expr]) ys
    _                                       -> Nothing
parseArgs tab tokens           = case parseAPrototype tokens of
    Just (expr, toks@(TokenClose: ys))      -> Just (tab ++ [expr], ys)
    Just (expr, toks@(TokenSep: ys))        -> parseArgs (tab ++ [expr]) ys
    _                                       -> Nothing  


parseDef :: Parser Expr
parseDef tokens = case parseName tokens of
    Just ((Var name _), xs) -> case parsePrototype [] xs of
        Just (proto, (TokenType:ys)) -> case parseValue ys of
            Just (x, rest) -> case parseBody ((specialTypeArgument proto) ++ [(name, (getTypeFromTypeName x))]) rest of
                Just (z, zs) -> Just (Function name proto z (getTypeFromTypeName x), zs)


parseCall :: String -> Parser Expr
parseCall name toks = case parseArgs [] toks of
    Just (xprs, rest)   -> Just (Call name xprs None, rest)
    _                   -> error (show toks)

parseExtern :: Parser Expr
parseExtern toks = case parseValue toks of
    Just ((Var name _), xs) -> case parsePrototype [] xs of
        Just (xprs, (TokenType:ys)) -> case parseValue ys of
            Just (x, rest) -> Just (Extern name xprs (getTypeFromTypeName x), rest) 
                
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
parseValue (Word n:xs@(y:ys))      | n == "for"    = parseFor xs
                            | n == "if"     = parseIf xs
                            | n == "while"  = parseWhile xs
                            | n == "def"    = parseDef xs
                            | n == "extern" = parseExtern xs
                            | otherwise     = case y of
                                TokenOpen   -> parseCall n ys
                                _           -> Just ((Var n None), xs)
            --
-- Error for parsing the value
parseValue x = error ("Token not recognize" ++ (show x))

parseBinOp :: Expr -> Op -> Parser Expr
parseBinOp previousExpr op tokens = case parseExpr tokens of
    Just (x, toks)      -> Just (BinOp op previousExpr x (getTypefromExpr x), toks)  
    _                   -> Nothing

parseExpr :: Parser Expr
parseExpr token = case parseValue token of
    Just (x , (TokenOp op:xs))              -> parseBinOp x op xs
    Just (x , (TokenAssign:xs))             -> parseAssign x xs
    Just ((Var x _), tokz@(TokenOpen:xs))   -> parseCall x xs
    -- Just (x, tokz@(TokenType:xs))           -> case parseExpr xs of
    --     Just (y, rest)  -> Just (List x y None, rest)
    Just (x , toks)                         -> Just (x, toks)

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
    -- Just (x, tokz@(TokenType:xs))           -> case parseExpr xs of
    --     Just (y, rest)  -> Just (List x y None, rest)    
    _                                       -> Nothing

findExprType :: [(String, ExprType)] -> String -> ExprType
findExprType a s = case find (\(k, _) -> s == s) (a) of
    Just (_, e) -> e
    Nothing -> None 

-- putArgIntoScoop :: [(String, ExprType)] -> [Expr] -> [(String, ExprType)]
-- putArgIntoScoop tab [] = tab
-- putArgIntoScoop tab ((Var name xtype):xs) = 


specialTypeArgument :: [Expr] -> [(String, ExprType)]
specialTypeArgument [] =  []
specialTypeArgument (expr@(Var name xtype):xs)  = (name, xtype) : specialTypeArgument  xs
specialTypeArgument _ = error "Bad call of the specialTypeArgument function"

specialTypeCall :: [(String, ExprType)] -> [Expr] -> [Expr]
specialTypeCall tab (expr@(Function name param body xtype):xs)  = expr : specialTypeCall (tab ++ [(name, xtype)]) xs --(Function name param (head (specialTypeCall (tab ++ (specialTypeArgument param)) [body])) xtype) : specialTypeCall (tab ++ [(name, xtype)]) xs
specialTypeCall tab (expr@(Assign (Var name s) x xtype):xs)     = Assign (Var name None) (head (specialTypeCall tab [x])) xtype: specialTypeCall (tab ++ [(name, xtype)]) xs
specialTypeCall tab (expr@(Var name xtype):xs)                  = setTypeExpr expr (findExprType tab name) : specialTypeCall tab xs
specialTypeCall tab (expr@(Call name x  y):xs)                  = setTypeExpr expr (findExprType tab name) : specialTypeCall tab xs
specialTypeCall tab (expr@(BinOp a x  y xtype):xs)              = BinOp a (head (specialTypeCall tab [x])) (head (specialTypeCall tab [y])) (getTypefromExpr ((head (specialTypeCall tab [x])))) : specialTypeCall tab xs
specialTypeCall tab (expr@(If a x  y xtype):xs)                 = case y of 
                                                                    Just z  -> If (head (specialTypeCall tab [a])) (head (specialTypeCall tab [x])) (Just (head (specialTypeCall tab [x]))) (getTypefromExpr ((head (specialTypeCall tab [x])))) : specialTypeCall tab xs
                                                                    _       -> If (head (specialTypeCall tab [a])) (head (specialTypeCall tab [x])) Nothing (getTypefromExpr ((head (specialTypeCall tab [x])))): specialTypeCall tab xs
specialTypeCall tab (expr@(UnaryOp a x     xtype):xs)           = UnaryOp a (head (specialTypeCall tab [x])) (getTypefromExpr ((head (specialTypeCall tab [x])))) : specialTypeCall tab xs
specialTypeCall tab (x:xs)  = x : specialTypeCall tab xs
specialTypeCall tab []  = []

parseBody :: [(String, ExprType)] -> Parser Expr
parseBody tab token = case parseValueBody tab token of
    Just (x , (TokenOp op:xs))              -> parseBinOpBody tab x op xs
    -- Just (x , (TokenType:xs))               -> parseBinOpBody tab x op xs
    Just (x , (TokenAssign:xs))             -> parseAssign x xs
    Just ((Var x _), tokz@(TokenOpen:xs))   -> parseCallBody tab x xs
    Just ((Var x _), xs)                    -> Just (Var x (findExprType tab x), xs)
    -- Just (x, tokz@(TokenType:xs))           -> case parseExpr xs of
    --     Just (y, rest)  -> Just (List x y None, rest)
    Just (x , toks)                         -> Just (x, toks)

parseBinOpBody :: [(String, ExprType)] -> Expr -> Op -> Parser Expr
parseBinOpBody tab previousExpr op tokens = case parseBody tab tokens of
    Just (x, toks)      -> Just (BinOp op previousExpr x (getTypefromExpr x), toks)  
    _                   -> Nothing

parseCallBody :: [(String, ExprType)] -> String -> Parser Expr
parseCallBody tab name toks = case parseArgs [] toks of
    Just (xprs, rest)   -> Just (Call name (specialTypeCall tab xprs) (findExprType tab name), rest)
    _                   -> error (show toks)


parseValueBody :: [(String, ExprType)] -> Parser Expr
parseValueBody tab (Value x:xs)         = Just ((Val x (typeValueToExpr x)), xs)
parseValueBody tab (TokenOpen : xs)     = case parseBody tab xs of
    Just (expr, (TokenClose : ys))  -> Just (expr, ys)
    Nothing                         -> error "Parse Value return nothing when token open is detected"
parseValueBody tab (TokenOp op: xs)     = parseUnOp op xs
--
parseValueBody tab (Word n:xs@(y:ys))   | n == "for"    = parseForBody tab xs
                                        | n == "if"     = parseIfBody tab xs
                                        | n == "while"  = parseWhileBody tab xs
                                        | otherwise     = case y of
                                        TokenOpen   -> parseCallBody tab n ys
                                        _           -> Just ((Var n None), xs)
            --
-- Error for parsing the value
parseValueBody tab x = error ("Token not recognize" ++ (show x))

parseForBody :: [(String, ExprType)] -> Parser Expr
parseForBody tab tokens = case parseBody tab tokens of
    Just (x, TokenSep:xs)   -> case parseBody tab xs of
        Just (y, TokenSep:ys)   -> case parseBody tab ys of
            Just (z, Word "in":zs)  -> case parseBody tab zs of
                Just (final, restoks) -> Just (For x y  (Just z) final (getTypefromExpr x), restoks)
                _                     -> error "Bad in parsing for expr"
            _                     -> error "Bad third sep parsing for expr"
        Just (z, Word "in":zs)  -> case parseBody tab zs of
            Just (final, restoks) -> Just (For x z Nothing final (getTypefromExpr x), restoks)
        _                     -> error "Bad second sep parsing for expr"
    _                     -> error "Bad first sep parsing for expr"

parseIfBody :: [(String, ExprType)] -> Parser Expr
parseIfBody tab tokens = case parseBody tab tokens of 
    Just (x, Word "then":xs)    ->  case parseBody tab xs of
        Just (y, Word "else":ys)    -> case parseBody tab ys of
            Just (z, zs)                -> Just (If x y (Just z) (getTypefromExpr y), zs)
        Just (y, ys)                -> Just (If x y Nothing (getTypefromExpr y), ys)
    _                           -> error "Bad If expressions"

parseWhileBody :: [(String, ExprType)] -> Parser Expr
parseWhileBody tab tokens = case parseBody tab tokens of
    Just (x, Word "do":xs) -> case parseBody tab xs of
        Just (y, yz)    -> Just (While x y (getTypefromExpr x), yz)

parseAPrototypeBody :: [(String, ExprType)] ->  Parser Expr
parseAPrototypeBody tab tok = case parseExpr tok of
    Just ((Var n _), TokenType:xs) -> case parseValue xs of
        Just (y, ys) -> Just (Var n (getTypeFromTypeName y), ys)
    Just ((Var n _), xs) -> Just (Var n (findExprType tab n), xs)
    Just (x, xs) -> Just (x, xs)

parsePrototypeBody :: [(String, ExprType)] -> [Expr] ->  Parser [Expr]
parsePrototypeBody mem tab toks@(TokenOpen:(TokenClose:ys)) = Just ([], ys)
parsePrototypeBody mem tab (TokenOpen:xs)   =  case parseAPrototypeBody mem xs of
    Just (expr, toks@(TokenClose: ys))      -> Just (tab ++ [expr], ys)
    Just (expr, toks)                       -> parsePrototypeBody mem (tab ++ [expr]) toks
    _                                       -> Nothing
parsePrototypeBody mem tab tokens           = case parseAPrototypeBody mem tokens of
    Just (expr, toks@(TokenClose: ys))      -> Just (tab ++ [expr], ys)
    Just (expr, toks)                       -> parsePrototypeBody mem (tab ++ [expr]) toks
    _                                       -> Nothing  
    

{-
Launch the expression's parsing instance
-}
createAst :: [Token] -> Expr
createAst [] = Ast [] None
createAst tokens = case parseExprs [] tokens of
    Just (result, [])   -> Ast (specialTypeCall [] result) None
    _                   -> error "bad parsing"