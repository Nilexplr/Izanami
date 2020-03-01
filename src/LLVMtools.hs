{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module LLVMtools where

import Parser
import Tokenize
import JIT
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.String
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as Text
import Foreign.Ptr
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.AST.Float
import LLVM.AST.FloatingPointPredicate hiding (False, True)
import qualified LLVM.AST.IntegerPredicate as Sicmp
import LLVM.AST.Operand
import LLVM.AST.Type as Type
import LLVM.IRBuilder
import LLVM.Module
import LLVM.PassManager
import LLVM.Pretty
import LLVM.Target
import System.IO
import System.IO.Error
import Data.Int
import Data.Word
import Control.Monad.Trans
import System.Console.Haskeline
import Control.Monad.Except
import Control.Monad.Fix
import LLVM.Target
import LLVM.CodeModel
import Numeric

type Binds = Map.Map String Operand

fromWhileToLLVM :: Expr -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
fromWhileToLLVM (While cond body ExprDouble) = mdo 
    preheaderB <- block `named` "preheader"
    initCondV <- (fromExprsToLLVM [cond] >>= fcmp ONE zero) `named` "initcond"
    condBr initCondV loopB afterB
    loopB <- block `named` "loop"
    -- build the body expression with 'i' in the bindings
    fromExprsToLLVM [body] `named` "body"
    -- default to 1 if there's no step defined
    let zero = ConstantOperand (Float (Double 0))
    -- again we need 'i' in the bindings
    condV <- (fromExprsToLLVM [cond] >>= fcmp ONE zero) `named` "cond"
    condBr condV loopB afterB
    afterB <- block `named` "after"
    -- since a for loop doesn't really have a value, return 0
    return $ ConstantOperand (Float (Double 0))
--
fromWhileToLLVM (While cond body ExprInt) = mdo 
    preheaderB <- block `named` "preheader"
    let init = case cond of 
            Val (ValueInt n) _  -> (icmp Sicmp.NE zero (ConstantOperand (Int (fromInteger (fromIntegral 1)) (fromIntegral n))))
            _                   -> (fromExprsToLLVM [cond] >>= icmp Sicmp.NE zero)
    initCondV <- init `named` "initcond"
    condBr initCondV loopB afterB
    loopB <- block `named` "loop"
    -- build the body expression with 'i' in the bindings
    fromExprsToLLVM [body] `named` "body"
    -- default to 1 if there's no step defined
    let zero = ConstantOperand (Int (fromInteger (fromIntegral 1)) (fromIntegral 0))
    -- again we need 'i' in the bindings
    condV <- (fromExprsToLLVM [cond] >>= icmp Sicmp.NE zero) `named` "cond"
    condBr condV loopB afterB
    afterB <- block `named` "after"
    -- since a for loop doesn't really have a value, return 0
    return $ ConstantOperand (Int (fromInteger (fromIntegral 32)) (fromIntegral 0))

fromForToLLVM :: Expr -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
fromForToLLVM (For (Assign (Var name nameType) expr ExprInt) cond step body xtype) = mdo
    preheaderB <- block `named` "preheader"
    initV <- fromExprsToLLVM [expr] `named` "init"
    -- build the condition expression with 'i' in the bindings
    initCondV <- withReaderT (Map.insert name initV) $
                    (fromExprsToLLVM [cond] >>= icmp Sicmp.NE zero) `named` "initcond"

    -- skip the loop if we don't meet the condition with the init
    condBr initCondV loopB afterB
    loopB <- block `named` "loop"

    i <- phi [(initV, preheaderB), (nextVar, loopB)] `named` "i"

    withReaderT (Map.insert name i) $ fromExprsToLLVM [body] `named` "body"

    stepV <- case step of
        Just steps -> fromExprsToLLVM [steps]
        Nothing -> return $ ConstantOperand (Int (fromInteger (fromIntegral 32)) (fromIntegral 1))
    nextVar <- add i stepV `named` "nextvar"
    let zero = ConstantOperand (Int (fromInteger (fromIntegral 1)) (fromIntegral 0))

    condV <- withReaderT (Map.insert name i) $
                (fromExprsToLLVM [cond] >>= icmp Sicmp.NE zero) `named` "cond"
    condBr condV loopB afterB

    afterB <- block `named` "after"

    return $ ConstantOperand (Int (fromInteger (fromIntegral 32)) (fromIntegral 0))


--
fromForToLLVM (For (Assign (Var name nameType) expr ExprDouble) cond step body xtype) = mdo
    preheaderB <- block `named` "preheader"
    initV <- fromExprsToLLVM [expr] `named` "init"

    -- build the condition expression with 'i' in the bindings
    initCondV <- withReaderT (Map.insert name initV) $
                  (fromExprsToLLVM [cond] >>= fcmp ONE zero) `named` "initcond"
    
    -- skip the loop if we don't meet the condition with the init
    condBr initCondV loopB afterB
    loopB <- block `named` "loop"
    
    i <- phi [(initV, preheaderB), (nextVar, loopB)] `named` "i"
    
    -- build the body expression with 'i' in the bindings
    withReaderT (Map.insert name i) $ fromExprsToLLVM [body] `named` "body"
    
    -- default to 1 if there's no step defined
    stepV <- case step of
      Just steps -> fromExprsToLLVM [steps]
      Nothing -> return $ ConstantOperand (Float (Double 1))
    nextVar <- fadd i stepV `named` "nextvar"
    
    let zero = ConstantOperand (Float (Double 0))
    -- again we need 'i' in the bindings
    condV <- withReaderT (Map.insert name i) $
              (fromExprsToLLVM [cond] >>= fcmp ONE zero) `named` "cond"
    condBr condV loopB afterB
    
    afterB <- block `named` "after"

    -- since a for loop doesn't really have a value, return 0
    return $ ConstantOperand (Float (Double 0))


fromIfToLLVM :: Expr -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
fromIfToLLVM (If cond thenexpr (Just elsexpr) xtype)= mdo
    ifB <- block `named` "if"

    let zero = ConstantOperand (Float (Double 0))
    condV <- fromExprsToLLVM [cond]
    cmp <- fcmp ONE zero condV `named` "cmp"

    condBr cmp thenB elseB

    thenB <- block `named` "then"
    thenOp <- fromExprsToLLVM [thenexpr]
    br mergeB   

    elseB <- block `named` "else"
    elseOp <- fromExprsToLLVM [elsexpr]
    br mergeB   

    mergeB <- block `named` "ifcont"
    phi [(thenOp, thenB), (elseOp, elseB)]
--
fromIfToLLVM (If cond thenexpr Nothing xtype)= mdo
    ifB <- block `named` "if"

    let zero = ConstantOperand (Float (Double 0))
    condV <- fromExprsToLLVM [cond]
    cmp <- fcmp ONE zero condV `named` "cmp"

    condBr cmp thenB mergeB

    thenB <- block `named` "then"
    thenOp <- fromExprsToLLVM [thenexpr]
    br mergeB      

    mergeB <- block `named` "ifcont"
    phi [(thenOp, thenB)]

fromASTToLLVM :: Expr -> ModuleBuilder Operand
fromASTToLLVM (Function nameS paramsS fbody ftype) = do
    let name = fromString nameS
    case ftype of
        ExprDouble  -> function name params Type.double $ \ops -> do
            let variables = Map.fromList (zip (varToString <$> paramsS) ops)
            flip runReaderT variables $ fromExprsToLLVM [fbody] >>= ret
        ExprInt     -> function name params Type.i32 $ \ops -> do
            let variables = Map.fromList (zip (varToString <$> paramsS) ops)
            flip runReaderT variables $ fromExprsToLLVM [fbody] >>= ret
        where params = createParam <$> paramsS


fromASTToLLVM (Ast ((Function nameS paramsS fbody ftype):[]) ty) = do
    let name = fromString nameS
    case ftype of
        ExprDouble  -> function name params Type.double $ \ops -> do
            let variables = Map.fromList (zip (varToString <$> paramsS) ops)
            flip runReaderT variables $ fromExprsToLLVM [fbody] >>= ret
        ExprInt     -> function name params Type.i32 $ \ops -> do
            let variables = Map.fromList (zip (varToString <$> paramsS) ops)
            flip runReaderT variables $ fromExprsToLLVM [fbody] >>= ret
        where params = createParam <$> paramsS

fromASTToLLVM (Ast ((Function nameS paramsS fbody ftype):rest@(x:xs)) ty) = do
    let name = fromString nameS
    case ftype of
        ExprDouble  -> function name params Type.double $ \ops -> do
            let variables = Map.fromList (zip (varToString <$> paramsS) ops)
            flip runReaderT variables $ fromExprsToLLVM [fbody] >>= ret
        ExprInt     -> function name params Type.i32 $ \ops -> do
            let variables = Map.fromList (zip (varToString <$> paramsS) ops)
            flip runReaderT variables $ fromExprsToLLVM [fbody] >>= ret
    fromASTToLLVM (Ast rest (getTypefromExpr x))
        where params = createParam <$> paramsS

fromASTToLLVM (Extern name params etype) = extern (fromString name) (exprTypeToType <$> (getTypefromExpr <$> params)) (exprTypeToType etype)
fromASTToLLVM (Ast x xtype) = function "__anon_expr" [] ty $
    const $ flip runReaderT mempty $ fromExprsToLLVM x >>= ret
        where
            ty  | xtype == ExprDouble   = Type.double
                | xtype == ExprInt      = Type.i32
                | xtype == None         = Type.i32
                | otherwise             = error ("Invalid Ast type " ++ (show xtype))

{-
|   @varToString
|   take a Var and return his name
-}
varToString :: Expr -> String
varToString (Var name _)    = name
varToString _               = error "Invalid var"

{-
|   @createParam
|   take the Expr param and return a tuple with the type and the name of the param
-}
createParam :: Expr -> (Type, ParameterName)
createParam (Var name ptype)    | ptype == ExprInt      = (Type.i32, fromString name)
                                | ptype == ExprDouble   = (Type.double, fromString name)
                                | otherwise             = error "Invalid parrameter type"
createParam _   = error "Invalid parrameter" 

{-
|   @fromExprsToLLVM
|   transform an Expr into a LLVM AST
-}
fromExprsToLLVM :: [Expr] -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
fromExprsToLLVM (xpr@(BinOp _ _ _ _):[])    = fromBinOpToLLVM xpr
fromExprsToLLVM (xpr@(Val _ xtype):[])      = fromValToLLVM xpr xtype
fromExprsToLLVM (xpr@(Call _ _ _):[])       = fromCallToLLVM xpr
fromExprsToLLVM (expr@(If _ _ _ _):[])      = fromIfToLLVM expr
fromExprsToLLVM (expr@(For _ _ _ _ _):[])   = fromForToLLVM expr
fromExprsToLLVM (expr@(While _ _ _):[])     = fromWhileToLLVM expr
fromExprsToLLVM (expr@(List xp1 xp2 _):[])  = do
                                        fromExprsToLLVM [xp1]
                                        fromExprsToLLVM [xp2]
fromExprsToLLVM (xpr@(Var name _):[])       = do
                                        variables <- ask
                                        case variables Map.!? name of
                                            Just v  -> pure v
                                            Nothing -> error "Undefined variable."
fromExprsToLLVM (xpr@(BinOp _ _ _ _):xs)    = do
                                        fromBinOpToLLVM xpr
                                        fromExprsToLLVM xs
fromExprsToLLVM (xpr@(Val _ xtype):xs)      = do
                                        fromValToLLVM xpr xtype
                                        fromExprsToLLVM xs
fromExprsToLLVM (xpr@(Call _ _ _):xs)       = do
                                        fromCallToLLVM xpr
                                        fromExprsToLLVM xs
fromExprsToLLVM (expr@(If _ _ _ _):xs)      = do
                                        fromIfToLLVM expr
                                        fromExprsToLLVM xs
fromExprsToLLVM (expr@(For _ _ _ _ _):xs)   = do
                                        fromForToLLVM expr
                                        fromExprsToLLVM xs
fromExprsToLLVM (expr@(While _ _ _):xs)     = do
                                        fromWhileToLLVM expr
                                        fromExprsToLLVM xs
fromExprsToLLVM (expr@(List xp1 xp2 _):xs)  = do
                                        fromExprsToLLVM [xp1]
                                        fromExprsToLLVM [xp2]
                                        fromExprsToLLVM xs
fromExprsToLLVM (xpr@(Var name _):xs)       = do
                                        variables <- ask
                                        case variables Map.!? name of
                                            Just v  -> pure v
                                            Nothing -> error "Undefined variable."
                                        fromExprsToLLVM xs
-- fromExprsToLLVM xpr@(Assign _ xp xtype)     = 
fromExprsToLLVM expr    = error ("Invalid expr " ++ (show expr))

{-
|   @fromCallToLLVM
|   transform Call expr into a LLVM function call
-}
fromCallToLLVM :: Expr -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
fromCallToLLVM (Call name paramsExpr ctype) = do
                                            params <- mapM fromExprsToLLVM (fromElemToArray <$> paramsExpr)
                                            let funcName    = fromString name
                                                ty          = FunctionType Type.i32 (exprTypeToType <$> (getTypefromExpr <$> paramsExpr)) False
                                                ptrTy       = Type.PointerType ty (AddrSpace 0)
                                                ref         = GlobalReference ptrTy funcName
                                            call (ConstantOperand ref) (zip params (repeat []))
fromCallToLLVM xpr = error ("Bad call : " ++ show xpr)

{-
|   @fromElemToArray
|   rturn elem into a array (for <$> purpose only)
-}
fromElemToArray :: a -> [a]
fromElemToArray x = [x]

{-
|   @exprTypeToType
|   turn an ExprType to a Type
-}
exprTypeToType :: ExprType -> Type
exprTypeToType ExprInt      = Type.i32
exprTypeToType ExprDouble   = Type.double

{-
|   @exprToFirstClass
|   turn an expr to a First Class
-}

{-
|   @fromAssignToLLVM
|   transform Assign expr into a LLVM assignation
-}

{-
|   @fromBinOpToLLVM
|   BinOp transformation from Expr to LLVM ast using LLVM instr (changing instr in function of type and op)
-}
fromBinOpToLLVM :: Expr -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
fromBinOpToLLVM (BinOp op xp1 xp2 xtype)   = do
    op1 <- fromValToLLVM xp1 xtype
    op2 <- fromValToLLVM xp2 xtype
    tmp <- instr op1 op2
    if isCmp && (xtype == ExprDouble) then uitofp tmp Type.double  else return tmp
        where
            isCmp
                | op == Inf = True
                | op == Sup = True
                | op == Eq  = True
                | op == Dif = True
                | otherwise = False
            instr = case op of
                Plus    -> case xtype of
                                    ExprInt     -> add
                                    ExprDouble  -> fadd
                                    _           -> error "Invalid type for binOp"
                Minus   -> case xtype of
                                    ExprInt     -> sub
                                    ExprDouble  -> fsub
                                    _           -> error "Invalid type for binOp"
                Time    -> case xtype of
                                    ExprInt     -> mul
                                    ExprDouble  -> fmul
                                    _           -> error "Invalid type for binOP"
                Div     -> case xtype of
                                    ExprInt     -> sdiv
                                    ExprDouble  -> fdiv
                                    _           -> error "Invalid type for binOP"
                Mod     -> case xtype of
                                    ExprInt     -> srem
                                    ExprDouble  -> frem
                                    _           -> error "Invalid type for binOP"
                Inf     -> case xtype of
                                    ExprInt     -> (icmp Sicmp.SLT)
                                    ExprDouble  -> (fcmp OLT)
                                    _           -> error "Invalid type for binOP"
                Sup     -> case xtype of
                                    ExprInt     -> (icmp Sicmp.SGT)
                                    ExprDouble  -> (fcmp OGT)
                                    _           -> error "Invalid type for binOP"
                -- InfEq   -> case xtype of
                --                     ExprInt     -> (icmp Sicmp.SLE)
                --                     ExprDouble  -> (fcmp OLE)
                --                     _           -> error "Invalid type for binOP"
                -- SupEq   -> case xtype of
                --                     ExprInt     -> (icmp Sicmp.SGE)
                --                     ExprDouble  -> (fcmp OGE)
                --                     _           -> error "Invalid type for binOP"
                Eq      -> case xtype of
                                    ExprInt     -> (icmp Sicmp.EQ)
                                    ExprDouble  -> (fcmp OEQ)
                                    _           -> error "Invalid type for binOP"
                Dif     -> case xtype of
                                    ExprInt     -> (icmp Sicmp.NE)
                                    ExprDouble  -> (fcmp ONE)
                                    _           -> error "Invalid type for binOP"
                _       -> error "Invalid op"

{-
|   @fromValToLLVM
|   transform a Val Expr into a LLVM ConstantOperand
-}
fromValToLLVM :: Expr -> ExprType -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
fromValToLLVM (Val vx vtype) ExprDouble     | vtype == ExprDouble   = pure $ ConstantOperand (Float (Double xd))
                                            | vtype == ExprInt      = pure $ ConstantOperand (Float (Double (fromIntegral xi)))
                                            | otherwise             = error "Invalid Value Type"
                                                                        where
                                                                            xd = case vx of
                                                                                ValueDouble y -> y
                                                                                _ -> error "Incompatible type"
                                                                            xi = case vx of
                                                                                ValueInt y -> y
                                                                                _ -> error "Incompatible type"
fromValToLLVM (Val vx vtype) ExprInt        | vtype == ExprInt      = pure $ ConstantOperand (Int (fromInteger (fromIntegral 32)) (fromIntegral xi))
                                            | vtype == ExprDouble   = pure $ ConstantOperand (Int (fromInteger (fromIntegral 32)) (fromIntegral (round xd :: Int)))
                                            | otherwise             = error "Invalid Value Type"
                                                                        where
                                                                            xd = case vx of
                                                                                ValueDouble y -> y
                                                                                _ -> error "Incompatible type"
                                                                            xi = case vx of
                                                                                ValueInt y -> y
                                                                                _ -> error "Incompatible type"
fromValToLLVM x _ = fromExprsToLLVM [x]