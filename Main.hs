
-- | A test script to check those build systems claiming to implement a test
--   do in fact do so.
module Main(main) where

import Util


main :: IO ()
main = do
    test "basic" basic
    test "parallel" parallel
    test "include" include
    test "wildcard" wildcard
    test "spaces" spaces
    test "monad1" monad1
    test "monad2" monad2
    test "monad3" monad3


basic :: ([Opt] -> IO ()) -> IO ()
basic run = do
    writeFile "input" "xyz"
    run [Contents "output" "xyz"]
    run [NoChange]
    writeFile "input" "abc"
    run [Contents "output" "abc"]
    run [NoChange]


parallel :: ([Opt] -> IO ()) -> IO ()
parallel run = do
    writeFile "input1" "xyz"
    writeFile "input2" "abc"
    run [Parallel 2, Contents ".log" "start\nstart\nend\nend\n"]
    run [NoChange]


include :: ([Opt] -> IO ()) -> IO ()
include run = do
    run [Change "main.o"]
    run [NoChange]
    touch "include-2.h"
    run [Change "main.o"]
    run [NoChange]


wildcard :: ([Opt] -> IO ()) -> IO ()
wildcard run = do
    x <- randomRIO (1::Int,1000000)
    let name = "name" ++ show x
    writeFile (name ++ ".in") "abc"
    run [Target $ name ++ ".out", Contents (name ++ ".out") "abc"]
    run [Target $ name ++ ".out", NoChange]
    writeFile (name ++ ".in") "xyz"
    run [Target $ name ++ ".out", Contents (name ++ ".out") "xyz"]
    run [Target $ name ++ ".out", NoChange]


spaces :: ([Opt] -> IO ()) -> IO ()
spaces run = do
    writeFile "input file" "abc"
    run [Target "output file", Contents "output file" "abc"]
    run [Target "output file", NoChange]
    writeFile "input file" "xyz"
    run [Target "output file", Contents "output file" "xyz"]
    run [Target "output file", NoChange]


monad1 :: ([Opt] -> IO ()) -> IO ()
monad1 run = do
    writeBinary "list" "input1\ninput2\n"
    writeFile "input1" "test"
    writeFile "input2" "again"
    run [Target "output", Contents "output" "testagain"]
    run [Target "output", NoChange]
    writeFile "input1" "more"
    run [Target "output", Contents "output" "moreagain"]
    run [Target "output", NoChange]
    writeBinary "list" "input1\n"
    run [Target "output", Contents "output" "more"]
    run [Target "output", NoChange]
    writeFile "input2" "x"
    run [Target "output", NoChange]


monad2 :: ([Opt] -> IO ()) -> IO ()
monad2 run = do
    writeBinary "source" "output1\noutput2\n"
    writeFile "input1" "test"
    writeFile "input2" "again"
    run [Target "output", Contents "output" "testagain", Contents ".log" "run\n"]
    run [Target "output", NoChange, Contents ".log" "run\n"]
    writeFile "input1" "more"
    run [Target "output", Contents "output" "moreagain"]
    run [Target "output", NoChange]
    writeBinary "source" "output1\n"
    run [Target "output", Contents "output" "more", Contents ".log" "run\nrun\n"]
    run [Target "output", NoChange]
    writeFile "input2" "x"
    run [Target "output", NoChange, Contents ".log" "run\nrun\n"]


monad3 :: ([Opt] -> IO ()) -> IO ()
monad3 run = do
    writeBinary "source" "output1\noutput2\n"
    writeFile "input1" "test"
    writeFile "input2" "again"
    run [Target "output", Contents "output" "testagain", Contents ".log" "run\n"]
    run [Target "output", NoChange, Contents ".log" "run\n", Missing "gen"]
    writeBinary "source" "gen\noutput2\n"
    run [Target "output", Contents "output" "Generated\nagain"]
    run [Target "output", NoChange]
