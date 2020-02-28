{-# LANGUAGE OverloadedStrings #-}

module Compile 
    ( 
    )
    where

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

compile :: String -> ModuleBuilderT (ReaderT JITEnv IO) ()
compile str = do
    liftIO $ print $ parseExprs $ stringToToken str
    anon <- isAnonExpr <$> hoist (fromASTToLLVM $ parseExprs $ stringToToken str)
    def <- mostRecentDef
    ast <- moduleSoFar "main"
    ctx <- lift $ asks jitEnvContext
    env <- lift ask
    liftIO $ withModuleFromAST ctx ast $ \mdl -> do
        Text.hPutStrLn stderr $ ppll def
        writeFile "temp.bs" (unpack $ ppll def)
        system "llc temp.bs && gcc -c temp.bs.s -o file.o && gcc file.o -o a.out && rm -f temp.bs temp.bs.s file.o || echo ERROR"
        let spec = defaultCuratedPassSetSpec { optLevel = Just 3 }
        -- this returns true if the module was modified
        withPassManager spec $ flip runPassManager mdl
        when anon (jit env mdl >>= hPrint stderr)
    when anon (removeDef def)
    where
        isAnonExpr (ConstantOperand (GlobalReference _ "__anon_expr")) = True
        isAnonExpr _ = False