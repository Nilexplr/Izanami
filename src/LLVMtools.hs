{-# LANGUAGE OverloadedStrings #-}

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
import LLVM.Target
import LLVM.CodeModel

type Binds = Map.Map String Operand

fromASTToLLVM :: Expr -> ModuleBuilder Operand
fromASTToLLVM (Function nameS paramsS@(x:xs) fbody ftype) = do
    let name = fromString nameS
    case ftype of
        ExprDouble -> function name params Type.double $ \ops -> do
            let variables = Map.fromList (zip (varToString <$> paramsS) ops)
            flip runReaderT variables $ fromExprsToLLVM fbody >>= ret
        where params = createParam <$> paramsS

-- buildAST (Function (Prototype nameStr paramStrs) body) = do
--   let n = fromString nameStr
--   function n params Type.double $ \ops -> do
--     let binds = Map.fromList (zip paramStrs ops)
--     flip runReaderT binds $ buildExpr body >>= ret
--   where params = zip (repeat Type.double) (map fromString paramStrs)

-- buildAST (Extern (Prototype nameStr params)) =
--   extern (fromString nameStr) (replicate (length params) Type.double) Type.double
fromASTToLLVM (Ast (x:xs) xtype) = function "__anon_expr" [] Type.double $
    const $ flip runReaderT mempty $ fromExprsToLLVM x >>= ret

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
fromExprsToLLVM :: Expr -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
fromExprsToLLVM xpr@(BinOp _ _ _ _)         = fromBinOpToLLVM xpr
fromExprsToLLVM xpr@(Val _ xtype)           = fromValToLLVM xpr xtype
fromExprsToLLVM xpr@(Var name _) = do
                                variables <- ask
                                case variables Map.!? name of
                                    Just v  -> pure v
                                    Nothing -> error "Undefined variable."
-- fromExprsToLLVM xpr@(Assign _ xp xtype)     = 
fromExprsToLLVM expr    = error "Invalid"

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
    return tmp
        where
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
fromValToLLVM x _ = fromExprsToLLVM x