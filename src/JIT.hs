{-# LANGUAGE OverloadedStrings #-}

module JIT where

import Utils
import Parser
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

-- data IONu = Int | Double

foreign import ccall "dynamic" mkFunDouble :: FunPtr (IO Double) -> IO Double
foreign import ccall "dynamic" mkFunInt :: FunPtr (IO Int) -> IO Int

jitd :: JITEnv -> Module -> IO Double
jitd JITEnv{jitEnvCompileLayer=compLayer, jitEnvModuleKey=mdlKey} mdl =
    withModule compLayer mdlKey mdl $ do
        mangled <- mangleSymbol compLayer "__anon_expr"
        Right (JITSymbol fPtr _) <- findSymbolIn compLayer mdlKey mangled False
        mkFunDouble (castPtrToFunPtr (wordPtrToPtr fPtr))

jiti :: JITEnv -> Module -> IO Int
jiti JITEnv{jitEnvCompileLayer=compLayer, jitEnvModuleKey=mdlKey} mdl =
    withModule compLayer mdlKey mdl $ do
        mangled <- mangleSymbol compLayer "__anon_expr"
        Right (JITSymbol fPtr _) <- findSymbolIn compLayer mdlKey mangled False
        mkFunInt (castPtrToFunPtr (wordPtrToPtr fPtr))


        -- case xtype of
        --     ExprInt     -> mkFunInt (castPtrToFunPtr (wordPtrToPtr fPtr))
        --     ExprDouble  -> mkFunDouble (castPtrToFunPtr (wordPtrToPtr fPtr))
        --     _           -> error "Invalid ExprType"