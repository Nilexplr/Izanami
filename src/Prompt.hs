module Prompt
    ( launchPrompt
    , displayEval
    )
    where

import JIT
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
import Text.Printf
import Prelude hiding (catch)
import Control.Exception
import System.Exit
import Data.Char
import Control.Monad

import Parser
import Tokenize
import Eval

handler :: SomeException -> IO ()
handler ex = putStrLn $ "*** ERROR : " ++ show ex

spehandler :: SomeException -> IO ()
spehandler ex = do 
    putStrLn $ "*** ERROR : " ++ (show ex)
    exitWith $ ExitFailure 84

prompt :: ModuleBuilderT (ReaderT JITEnv IO) ()
prompt = do
    liftIO $ hPutStr stderr "KOAK>>> "
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
                -- Uncomment for print LLVM code:
                -- Text.hPutStrLn stderr $ ppll def
                let spec = defaultCuratedPassSetSpec { optLevel = Just 3 }
                -- this returns true if the module was modified
                withPassManager spec $ flip runPassManager mdl
                when anon (jit env mdl >>= hPrint stderr)
            when anon (removeDef def)
            prompt
    where
        eofHandler e
        | isEOFError e = return Nothing
        | otherwise = ioError e
        isAnonExpr (ConstantOperand (GlobalReference _ "__anon_expr")) = True
        isAnonExpr _ = False

displayEval :: AccessMemory -> String -> IO()
displayEval ram files = do
    catch (putStrLn $ displayExpr $ giveExpr $ evalExpr ram $ expr) spehandler
        where 
            expr   = last $ parseExpr $ stringToToken $ files

launchPrompt :: AccessMemory -> IO()
launchPrompt ram = do
        putStr "> " >> hFlush stdout
        out <- getLine
        let (new, expr) = (evalExpr ram ((parseExpr $ stringToToken $ out) !! 0))
        catch (putStrLn $ displayExpr $ expr) handler
        launchPrompt new