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


foreign import ccall "dynamic" mkFun :: FunPtr (IO Double) -> IO Double

data JITEnv = JITEnv
  { jitEnvContext :: Context
  , jitEnvCompileLayer :: IRCompileLayer ObjectLinkingLayer
  , jitEnvModuleKey :: ModuleKey
  }

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
            ast <- parseExprs $ stringToToken l
            anon <- isAnonExpr <$> hoist (buildAST ast)
            def <- mostRecentDef
            ctx <- lift $ asks jitEnvContext
            env <- lift ask
            liftIO $ withModuleFromAST ctx ast $ \mdl -> do
                Text.hPutStrLn stderr $ ppll def
                let spec = defaultCuratedPassSetSpec { optLevel = Just 3 }
                -- this returns true if the module was modified
                withPassManager spec $ flip runPassManager mdl
                when anon (jit env mdl >>= hPrint stderr)
            when anon (removeDef def)
        --   case readMaybe l of
        --     Nothing ->  liftIO $ hPutStrLn stderr "Couldn't parse"
        --     Just ast -> do
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

buildAST :: Expr -> ModuleBuilder Operand
-- buildAST (Function (Prototype nameStr paramStrs) body) = do
--   let n = fromString nameStr
--   function n params Type.double $ \ops -> do
--     let binds = Map.fromList (zip paramStrs ops)
--     flip runReaderT binds $ buildExpr body >>= ret
--   where params = zip (repeat Type.double) (map fromString paramStrs)

-- buildAST (Extern (Prototype nameStr params)) =
--   extern (fromString nameStr) (replicate (length params) Type.double) Type.double
buildAST (Start x) = function "__anon_expr" [] Type.double $
  const $ flip runReaderT mempty $ buildExprs x >>= ret

buildExprs :: [Expr] -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
buildExprs ((Val (TInt x)):xs) = pure $ ConstantOperand (Float (Double (fromIntegral x)))
buildExprs ((BinOp op xp1 xp2):xs) = do
    op1 <- buildExprs [xp1]
    op2 <- buildExprs [xp1]
    tmp <- fadd op1 op2
    return tmp