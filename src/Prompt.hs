module Prompt
    ( 
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

handler :: SomeException -> IO ()
handler ex = putStrLn $ "*** ERROR : " ++ show ex

spehandler :: SomeException -> IO ()
spehandler ex = do 
    putStrLn $ "*** ERROR : " ++ (show ex)
    exitWith $ ExitFailure 84
