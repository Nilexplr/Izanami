module Prompt
    ( launchPrompt
    , displayEval
    )
    where

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