{-# LANGUAGE OverloadedStrings #-}

module LLVMtools where

import Parser
import Tokenize
import Utils
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
import Data.Text.Lazy
import Control.Monad.Trans
import System.Console.Haskeline
import Control.Monad.Except
import LLVM.Target
import LLVM.CodeModel

type Binds = Map.Map String Operand

fromASTToLLVM :: Expr -> ModuleBuilder Operand
-- buildAST (Function (Prototype nameStr paramStrs) body) = do
--   let n = fromString nameStr
--   function n params Type.double $ \ops -> do
--     let binds = Map.fromList (zip paramStrs ops)
--     flip runReaderT binds $ buildExpr body >>= ret
--   where params = zip (repeat Type.double) (map fromString paramStrs)

-- buildAST (Extern (Prototype nameStr params)) =
--   extern (fromString nameStr) (replicate (length params) Type.double) Type.double
fromASTToLLVM x = function "__anon_expr" [] Type.double $
    const $ flip runReaderT mempty $ fromExprsToLLVM [x] >>= ret

fromExprsToLLVM :: [Expr] -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
fromExprsToLLVM ((Val (ValueInt x) ExprInt):xs) = pure $ ConstantOperand (Float (Double (fromIntegral x)))
fromExprsToLLVM ((BinOp op xp1 xp2 ExprInt):xs) = do
    op1 <- fromExprsToLLVM [xp1]
    op2 <- fromExprsToLLVM [xp2]
    tmp <- instr op1 op2
    return tmp
        where
            instr = case op of
                Plus -> fadd
                Minus -> fsub