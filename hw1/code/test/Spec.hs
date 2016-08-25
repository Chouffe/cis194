import Test.HUnit
import Control.Monad (void)

import Lib
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , hanoi
  , validate
  )

tests = test [ "toDigits" ~: "toDigits 1234" ~: [1, 2, 3, 4] ~=? toDigits 1234
             , "toDigits" ~: "toDigits 0" ~: [] ~=? toDigits 0
             , "toDigits" ~: "toDigits (-17)" ~: [] ~=? toDigits (-17)
             , "toDigitsRev" ~: "toDigitsRev 1234" ~: [4, 3, 2, 1] ~=? toDigitsRev 1234
             , "toDigitsRev" ~: "toDigitsRev 0" ~: [] ~=? toDigitsRev 0
             , "toDigitsRev" ~: "toDigitsRev (-17)" ~: [] ~=? toDigitsRev (-17)
             , "doubleEveryOther" ~: "doubleEveryOther [8,7,6,5]" ~: [16, 7, 12, 5] ~=? doubleEveryOther [8, 7, 6, 5]
             , "doubleEveryOther" ~: "doubleEveryOther [1,2,3]" ~: [1,4,3] ~=? doubleEveryOther [1,2,3]
             , "sumDigits" ~: "sumDigits [16, 7, 12, 5]" ~: 22  ~=? sumDigits [16, 7, 12, 5]
             , "validate" ~: "validate 4012888888881881" ~: True  ~=? validate 4012888888881881
             , "validate" ~: "validate 4012888888881882" ~: False ~=? validate 4012888888881882
             , "hanoi" ~: "hanoi 2 \"a\" \"b\" \"c\" " ~: [("a","c"), ("a","b"), ("c","b")] ~=?hanoi 2 "a" "b" "c"
             ]

main :: IO ()
main = void $ runTestTT tests
