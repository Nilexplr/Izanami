module Argument
    ( ArgumentType(..)
    , Options(..)
    , handleArgument
    ) where

{-
Options data declaration
-}
data Options = Options
    { pathFile          :: [String]
    , interactive       :: Bool
    } deriving Show

{-
Return a default Options data
-}
startOption :: Options
startOption = Options
    { pathFile          = []
    , interactive       = True
    }

{--
Declaration of the Flag datatype used for GetOpt
--}
data ArgumentType =     Invalid
                    |   Helper
                    |   Version
                    |   Other
    deriving (Show, Enum)

{-
Main function of the argument handling
-}
handleArgument :: [String] -> IO (Either ArgumentType Options)
handleArgument args = case parseArgument args of
                    Right   t           -> do return $ Right t
                    Left    (Helper)    -> do programUsage ; return $ Left Helper
                    Left    (Version)   -> do programVersion ; return $ Left Version
                    Left    (_)         -> do programInvalidArgs ; return $ Left Invalid


{-
Check if interactive flag is on
-}
isInteractive :: [String] -> Bool
isInteractive [] = False
isInteractive ("-i":xs) = True 
isInteractive (x:xs) = isInteractive xs

{-
Check if there is an invalid arguments
-}
isFlag :: [String] -> Bool
isFlag [] = True
isFlag (x:xs)   | x == "-i"     = isFlag xs
                | x !! 0 == '-' = False
                | otherwise     = isFlag xs

{-
Delete Flag from argument list
-}
deleteArgumentFlag :: [String] -> [String]
deleteArgumentFlag [] = []
deleteArgumentFlag ("-i":xs) = deleteArgumentFlag xs
deleteArgumentFlag (x:xs) = x : deleteArgumentFlag xs

{-
Return a String of file paths if no invalid argument detected
-}
getFiles :: [String] -> Maybe [String]
getFiles [] = Just []
getFiles x = case isFlag $ deleteArgumentFlag $ x of
    True -> Just $ deleteArgumentFlag $ x
    False -> Nothing
{-
Return the parsed argument
-}
parseArgument :: [String] -> Either ArgumentType Options
parseArgument []                = Right Options { pathFile = [], interactive = True }
parseArgument ["-i"]            = Right Options { pathFile = [], interactive = True }
parseArgument ["--help"]        = Left  Helper
parseArgument ["--version"]     = Left  Version
parseArgument x                 = case getFiles x of
                Just  a     -> Right Options { pathFile = a, interactive = isInteractive x }
                Nothing     -> Left Invalid

{-
Display the usage
-}
programUsage :: IO ()
programUsage = do putStrLn "./koak [FILE.k] [-i]"


{-
Display the program's version
-}
programVersion :: IO ()
programVersion = do putStrLn "koak v0.0.1"

{-
Display invalid message error,
-}
programInvalidArgs :: IO ()
programInvalidArgs = do putStrLn "the given arguments are invalid, please use the --help option"