module Main

import Test.Parser

import Tester.Runner

public export
main : IO ()
main = do
    success <- runTests $ testParser
    if success
        then putStrLn "All testHeading passed"
        else putStrLn "Not all testHeading passed"