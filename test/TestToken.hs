module TestToken (testToken) where

import Test.QuickCheck
import Tokenize
import Control.Exception

testToken :: IO ()
testToken = do
    quickCheck((stringToToken "+-()/^* != == = ! ") == [ TokenOp Plus
                                            , TokenOp Minus
                                            , TokenOpen
                                            , TokenClose
                                            , TokenOp Div
                                            , TokenOp Power
                                            , TokenOp Time
                                            , TokenOp Dif
                                            , TokenOp Eq
                                            , TokenOp Assign
                                            , TokenOp Not                                            
                                            ])
    quickCheck(stringToToken "2"    == [Number (Integer 2)])
    quickCheck(stringToToken "2.5"  == [Number (Decimal 2.5)])
    quickCheck(stringToToken "toto" == [Word "toto"])
    quickCheck(stringToToken "!toto" == [TokenOp Not, Word "toto"])
