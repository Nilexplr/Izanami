{-# LANGUAGE OverloadedStrings #-}

module JIT 
    ( runKoak
    , jit
    )
    where

import Prompt
import Compile
import Utils
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.String
import Foreign.Ptr
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

data JITEnv = JITEnv
    { jitEnvContext :: Context
    , jitEnvCompileLayer :: IRCompileLayer ObjectLinkingLayer
    , jitEnvModuleKey :: ModuleKey
    }

runKoak :: Bool -> String -> IO ()
runKoak isPrompt content = do
    withContext $ \ctx -> withHostTargetMachine $ \tm -> do
        withExecutionSession $ \exSession ->
            withSymbolResolver exSession (SymbolResolver symResolver) $ \symResolverPtr ->
                withObjectLinkingLayer exSession (const $ pure symResolverPtr) $ \linkingLayer ->
                    withIRCompileLayer linkingLayer tm $ \compLayer -> do
                        withModuleKey exSession $ \mdlKey -> if isPrompt == True
                            then do
                                let env = JITEnv ctx compLayer mdlKey
                                ast <- runReaderT (buildModuleT "main" prompt) env
                                return ()
                            else do
                                let env = JITEnv ctx compLayer mdlKey
                                ast <- runReaderT (buildModuleT "main" compile) env
                                return ()

symResolver :: MangledSymbol -> IO (Either JITSymbolError JITSymbol)
symResolver sym = undefined

foreign import ccall "dynamic" mkFun :: FunPtr (IO Double) -> IO Double

jit :: JITEnv -> Module -> IO Double
jit JITEnv{jitEnvCompileLayer=compLayer, jitEnvModuleKey=mdlKey} mdl =
    withModule compLayer mdlKey mdl $ do
        mangled <- mangleSymbol compLayer "__anon_expr"
        Right (JITSymbol fPtr _) <- findSymbolIn compLayer mdlKey mangled False
        mkFun (castPtrToFunPtr (wordPtrToPtr fPtr))