import Test.HUnit
import Control.Monad (void)

import Log
import LogAnalysis
    ( parseMessage )

tests :: Test
tests = test [ "parseMessage" ~: "parseMessage \"E 2 562 help help\"" ~: LogMessage (Error 2) 562 "help help" ~=? parseMessage "E 2 562 help help"
             , "parseMessage" ~: "parseMessage \"I 29 la la la\"" ~:  LogMessage Info 29 "la la la" ~=? parseMessage "I 29 la la la"
             , "parseMessage" ~: "parseMessage \"This is not in the right format\"" ~:  Unknown "This is not in the right format" ~=? parseMessage "This is not in the right format"
             ]

main :: IO ()
main = void $ runTestTT tests
