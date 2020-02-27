{-# LANGUAGE OverloadedStrings #-}

module LLVMtools 
    ( 
    )
    where

import Parser
import Tokenize
import Utils
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
import LLVM.Context
import LLVM.IRBuilder
import LLVM.Module
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import LLVM.PassManager
import LLVM.Pretty
import LLVM.Target
import System.IO
import System.IO.Error
import Text.Read (readMaybe)

import qualified LLVM.ExecutionEngine as EE

-- foreign import ccall "dynamic" mkFun :: FunPtr (IO Double) -> IO Double

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Trans
import System.Console.Haskeline
import Control.Monad.Except
import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

data JITEnv = JITEnv
  { jitEnvContext :: Context
  , jitEnvCompileLayer :: IRCompileLayer ObjectLinkingLayer
  , jitEnvModuleKey :: ModuleKey
  }

foreign import ccall "dynamic" mkFun :: FunPtr (IO Double) -> IO Double
 
test :: IO ()
test = do
  withContext $ \ctx -> withHostTargetMachine $ \tm -> do
    withExecutionSession $ \exSession ->
      withSymbolResolver exSession (SymbolResolver symResolver) $ \symResolverPtr ->
        withObjectLinkingLayer exSession (const $ pure symResolverPtr) $ \linkingLayer ->
          withIRCompileLayer linkingLayer tm $ \compLayer -> do
            withModuleKey exSession $ \mdlKey -> do
              let env = JITEnv ctx compLayer mdlKey
              ast <- runReaderT (buildModuleT "main" repl) env
              return ()
              
symResolver :: MangledSymbol -> IO (Either JITSymbolError JITSymbol)
symResolver sym = undefined

repl :: ModuleBuilderT (ReaderT JITEnv IO) ()
repl = do
  liftIO $ hPutStr stderr "ready> "
  mline <- liftIO $ catchIOError (Just <$> getLine) eofHandler
  case mline of
    Nothing -> return ()
    Just l -> do
      liftIO $ print $ parseExprs $ stringToToken l
      anon <- isAnonExpr <$> hoist (fromASTToLLVM $ parseExprs $ stringToToken l)
      def <- mostRecentDef
      
      ast <- moduleSoFar "main"
      ctx <- lift $ asks jitEnvContext
      env <- lift ask
      liftIO $ withModuleFromAST ctx ast $ \mdl -> do
        Text.hPutStrLn stderr $ ppll def
        let spec = defaultCuratedPassSetSpec { optLevel = Just 3 }
        -- this returns true if the module was modified
        withPassManager spec $ flip runPassManager mdl
        when anon (jit env mdl >>= hPrint stderr)

      when anon (removeDef def)
      repl
  where
    eofHandler e
      | isEOFError e = return Nothing
      | otherwise = ioError e
    isAnonExpr (ConstantOperand (GlobalReference _ "__anon_expr")) = True
    isAnonExpr _ = False

jit :: JITEnv -> Module -> IO Double
jit JITEnv{jitEnvCompileLayer=compLayer, jitEnvModuleKey=mdlKey} mdl =
  withModule compLayer mdlKey mdl $ do
    mangled <- mangleSymbol compLayer "__anon_expr"
    Right (JITSymbol fPtr _) <- findSymbolIn compLayer mdlKey mangled False
    mkFun (castPtrToFunPtr (wordPtrToPtr fPtr))

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
fromExprsToLLVM ((Val (TInt x)):xs) = pure $ ConstantOperand (Float (Double (fromIntegral x)))
fromExprsToLLVM ((BinOp op xp1 xp2):xs) = do
    op1 <- fromExprsToLLVM [xp1]
    op2 <- fromExprsToLLVM [xp2]
    tmp <- instr op1 op2
    return tmp
      where
        instr = case op of
          Plus -> fadd
          Minus -> fsub