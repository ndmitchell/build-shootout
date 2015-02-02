#!/usr/bin/env runhaskell

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
    test "unchanged" unchanged
    test "multiple" multiple
    test "system1" system1
    test "system2" system2
    test "pool" pool
    test "digest" digest
    test "nofileout" nofileout
    test "noleftover" noleftover
    test "secondary" secondary
    test "intermediate" intermediate


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
    run [Parallel 2, Log "start start end end"]
    run [NoChange]


include :: ([Opt] -> IO ()) -> IO ()
include run = do
    run [Change "main.o"]
    run [NoChange]
    appendFile "include-2.h" "\n/* comment */"
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
    run [Target "output", Contents "output" "testagain", Log "run"]
    run [Target "output", NoChange, Log "run"]
    writeFile "input1" "more"
    run [Target "output", Contents "output" "moreagain"]
    run [Target "output", NoChange]
    writeBinary "source" "output1\n"
    run [Target "output", Contents "output" "more", Log "run run"]
    run [Target "output", NoChange]
    writeFile "input2" "x"
    run [Target "output", NoChange, Log "run run"]


monad3 :: ([Opt] -> IO ()) -> IO ()
monad3 run = do
    writeBinary "source" "output1\noutput2\n"
    writeFile "input1" "test"
    writeFile "input2" "again"
    run [Target "output", Contents "output" "testagain", Log "run"]
    run [Target "output", NoChange, Log "run", Missing "gen"]
    writeBinary "source" "gen\noutput2\n"
    run [Target "output", Contents "output" "Generated\nagain"]
    run [Target "output", NoChange]


unchanged :: ([Opt] -> IO ()) -> IO ()
unchanged run = do
    writeFile "input" "foo is in here"
    run [Target "output", Contents "source" "foo is out here", Contents "output" "foo xs out here", Log "run"]
    run [Target "output", NoChange]
    writeFile "input" "bar is in here"
    run [Target "output", Contents "source" "bar is out here", Contents "output" "bar xs out here", Log "run run"]
    run [Target "output", NoChange]
    writeFile "input" "bar is out here"
    run [Target "output", Contents "source" "bar is out here", Contents "output" "bar xs out here", Log "run run"]
    run [Target "output", NoChange]


multiple :: ([Opt] -> IO ()) -> IO ()
multiple run = do
    writeFile "input" "abbc"
    run [Target "output1", Target "output2", Contents "output1" "AbbC", Contents "output2" "aBBC", Log "run run"]
    run [Target "output1", Target "output2", NoChange]
    writeFile "input" "aBBc"
    run [Target "output1", Target "output2", Contents "output1" "ABBC", Contents "output2" "aBBC", Log "run run run"]
    run [Target "output1", NoChange]
    writeFile "input" "ab"
    run [Target "output1", Contents "output1" "Ab", Contents "output2" "aBBC"]
    run [Target "output2", Contents "output1" "Ab", Contents "output2" "aB"]
    run [Target "output1", Target "output2", NoChange]


system1 :: ([Opt] -> IO ()) -> IO ()
system1 run = do
    writeFile "system1-data" "foo"
    writeFile "source" "none"
    run [Target "output", Contents "output" "foo", Log "gen run"]
    run [Target "output", Contents "output" "foo", Log "gen run gen"]
    writeFile "system1-data" "bar"
    run [Target "output", Contents "output" "bar", Log "gen run gen gen run"]


system2 :: ([Opt] -> IO ()) -> IO ()
system2 run = do
    let varName = "SYSTEM2_DATA"
    run [Contents "output" "", Log "run"]
    run [NoChange]
    run [Contents "output" "foo", Log "run run", Env varName "foo"]
    run [NoChange, Env varName "foo"]
    run [Contents "output" "bar", Log "run run run", Env varName "bar"]
    run [NoChange, Env varName "bar"]
    run [Contents "output" "", Log "run run run run"]
    run [NoChange]


pool :: ([Opt] -> IO ()) -> IO ()
pool run = do
    writeFile "input1" "xyz"
    writeFile "input2" "abc"
    writeFile "input3" "def"
    run [Parallel 8, Log "start start end start end end"]
    run [NoChange]


digest :: ([Opt] -> IO ()) -> IO ()
digest run = do
    writeFile "input" "xyz"
    run [Contents "output" "xyz"]
    run [NoChange]
    writeFile "input" "abc"
    run [Contents "output" "abc"]
    run [NoChange]
    writeFile "input" "abc"
    run [NoChange]


nofileout :: ([Opt] -> IO ()) -> IO ()
nofileout run = do
    writeFile "input" "xyz"
    run [Log "xyz"]
    run [NoChange]
    writeFile "input" "abc"
    run [Log "xyzabc"]
    run [NoChange]


noleftover :: ([Opt] -> IO ()) -> IO ()
noleftover run = do
    writeFile "foo.in" "foo"
    writeFile "bar.in" "bar"
    run [Contents "foo.out" "foo", Contents "bar.out" "bar"]
    run [NoChange]
    removeFile "bar.in"
    writeFile "baz.in" "baz"
    run [Contents "foo.out" "foo", Contents "baz.out" "baz", Missing "bar.out"]
    run [NoChange]


secondary :: ([Opt] -> IO ()) -> IO ()
secondary run = do
    writeFile "input" "xyz"
    run [Contents "output" "xyz * *", Contents "secondary" "xyz *", Log "run run"]
    run [NoChange]
    removeFile "secondary"
    run [Contents "output" "xyz * *", Missing "secondary", Log "run run"]
    run [NoChange]
    writeFile "input" "abc"
    run [Contents "output" "abc * *", Contents "secondary" "abc *", Log "run run run run"]
    run [NoChange]


intermediate :: ([Opt] -> IO ()) -> IO ()
intermediate run = do
    writeFile "input" "xyz"
    run [Contents "output" "xyz * *", Missing "intermediate", Log "run run"]
    run [NoChange]
    writeFile "input" "abc"
    run [Contents "output" "abc * *", Missing "intermediate", Log "run run run run"]
    run [NoChange]
