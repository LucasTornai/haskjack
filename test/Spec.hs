module Test where

import Haskjack hiding (main)
    
import Test.Tasty
import Test.Tasty.HUnit
    
main :: IO ()
main = do
    defaultMain tests
    
    
tests :: TestTree
tests = (testGroup "Tests" [handValueTest, doMoveTest])
    
                 
handValueTest = testGroup "handValue" 
            [testCase "Number Hand" (assertEqual "Test 1" 15 (handValue [Num 3, Num 4, Num 2, Num 6])),
             testCase "JQK" (assertEqual "Test 2" 30 (handValue [Jack, Queen, King])),
             testCase "Ace < 10" (assertEqual "Test 3" 16 (handValue [Num 5, Ace])),
             testCase "Ace 10" (assertEqual "Test 4" 21 (handValue [Num 3, Num 7, Ace])),
             testCase "Ace > 10" (assertEqual "Test 5" 13 (handValue [Num 3, Num 7, Num 2, Ace]))
            ]

doMoveTest = testGroup "doMove" 
            [testCase "Hit" (assertEqual "Test 1" ([Ace, Jack], [Num 10]) (doMove [Num 10, Ace, Jack] [] Hit)),
             testCase "Stand" (assertEqual "Test 1" ([Num 10, Ace, Jack], []) (doMove [Num 10, Ace, Jack] [] Stand))
            ]
    
    