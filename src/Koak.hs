{-# LANGUAGE OverloadedStrings #-}

module Koak where

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
import System.Process
import Text.Read (readMaybe)
import Data.Text.Lazy
import Parser
import LLVMtools
import Tokenize
import JIT
import Utils

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
                                writeFile "temp.bs" ""
                                let env = JITEnv ctx compLayer mdlKey
                                print $ (createAst (stringToToken content))
                                ast <- runReaderT (buildModuleT "main" (compileKoak (createAst (stringToToken content)))) env
                                system "llc temp.bs && gcc -c -g3 temp.bs.s -o file.o && gcc file.o main.o -o a.out || rm -f temp.bs temp.bs.s file.o || echo ERROR"
                                return ()

symResolver :: MangledSymbol -> IO (Either JITSymbolError JITSymbol)
symResolver sym = undefined

compileKoak :: Expr -> ModuleBuilderT (ReaderT JITEnv IO) ()
compileKoak (Ast astXpr@(x@(Function _ _ _ _):[]) _)        = appendCompileFile x
compileKoak (Ast astXpr@(x@(Function _ _ _ _):xs) atype)    = do
    appendCompileFile x
    compileKoak (Ast xs atype)
compileKoak (Ast astXpr@(x@(Extern _ _ _):[]) _)            = appendCompileFile x
compileKoak (Ast astXpr@(x@(Extern _ _ _):xs) atype)        = do
    appendCompileFile x
    compileKoak (Ast xs atype)
compileKoak (Ast astXpr@(x:xs) atype)                       = appendCompileFile (Ast astXpr (getTypefromExpr x))


appendCompileFile :: Expr -> ModuleBuilderT (ReaderT JITEnv IO) ()
appendCompileFile x = do
    -- Uncomment for print AST:
    -- liftIO $ print $ x
    anon <- isAnonExpr <$> hoist (fromASTToLLVM x)
    def <- mostRecentDef
    ast <- moduleSoFar "main"
    ctx <- lift $ asks jitEnvContext
    env <- lift ask
    liftIO $ withModuleFromAST ctx ast $ \mdl -> do
        -- Uncomment for print LLVM code:
        Text.hPutStrLn stderr $ ppll def
        appendFile "temp.bs" (unpack (ppll def))
        -- let spec = defaultCuratedPassSetSpec { optLevel = Just 3 }
        -- this returns true if the module was modified
        -- withPassManager spec $ flip runPassManager mdl
        -- when anon (jitd env mdl >>= hPrint stderr)
    when anon (removeDef def)
    where
        isAnonExpr (ConstantOperand (GlobalReference _ "__anon_expr")) = True
        isAnonExpr _ = False

prompt :: ModuleBuilderT (ReaderT JITEnv IO) ()
prompt = do
    liftIO $ hPutStr stderr "KOAK>>> "
    mline <- liftIO $ catchIOError (Just <$> getLine) eofHandler
    case mline of
        Nothing -> return ()
        Just "" -> prompt
        Just l -> do
            let ast     = createAst $ stringToToken l
            let xtype   = case ast of
                            Ast (x:xs) _ -> getTypefromExpr x 
            -- Uncomment for print AST:
            liftIO $ print ast
            anon <- isAnonExpr <$> hoist (fromASTToLLVM $ createAst $ stringToToken l)
            def <- mostRecentDef
            ast <- moduleSoFar "main"
            ctx <- lift $ asks jitEnvContext
            env <- lift ask
            liftIO $ withModuleFromAST ctx ast $ \mdl -> do
                -- Uncomment for print LLVM code:
                Text.hPutStrLn stderr $ ppll def
                let spec = defaultCuratedPassSetSpec { optLevel = Just 3 }
                -- this returns true if the module was modified
                withPassManager spec $ flip runPassManager mdl
                when anon (if xtype == ExprDouble then (jitd env mdl >>= hPrint stderr) else (jiti env mdl >>= hPrint stderr))
            when anon (removeDef def)
            prompt
    where
        eofHandler e
            | isEOFError e = return Nothing
            | otherwise = ioError e
        isAnonExpr (ConstantOperand (GlobalReference _ "__anon_expr")) = True
        isAnonExpr _ = False
