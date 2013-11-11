# Build System Shootout

[![Build Status](https://travis-ci.org/ndmitchell/build-shootout.png)](https://travis-ci.org/ndmitchell/build-shootout)

This project attempts to clarify the relative power of various build systems. Compared to the [Computer Language Shootout](http://benchmarksgame.alioth.debian.org/), this Shootout attempts to answer whether a build system is capable of expressing a particular dependency structure, but does not measure performance. The following build systems have at least one entry:

* [Make](http://www.gnu.org/software/make/) (GNU version), cross-platform
* [Ninja](http://martine.github.io/ninja/), cross-platform
* [Shake](https://github.com/ndmitchell/shake#readme), cross-platform

Below are a list of tests, a description of the test, and a list of build systems that can implement the test, and a list of those that are currently believed to lack the power to implement the test. The tests have pseudo-code for the equivalent untracked straight-line shell script.

To pass a test the build system must:

* Follow the specification, including the test case. _Warning:_ this project is young, the specifications and tests are still evolving slightly.
* Must not rebuild things in subsequent runs, all files must end up clean.
* Must not require explicit assume dirty/assume clean flags to be specified.
* Must not explicitly check for the existence of a file (you can always write a build system in a line of shell for any of these problems, the idea is to use the build system).
* All build-shootout shell scripts must be treated as black boxes. any file listed before `--` is treated as an input, any file after is an output. Functions like `gcc`, `cat`, `xargs` and `cp` are the standard Posix utilities.

If a build system requires restarting, which requires rechecking all previously checked dependency files but not running any expensive commands, it is considered a partial pass.

Performance is deliberately not measured as all actions are specified via shell scripts to make the results as similar as possible - even if some of the build systems would not use that approach.

### basic: Basic dependency

Given an input file, create an output file which is a copy of the input file. If the input file changes, or the output file is not present, the output file should be created.

    basic-run input -- output

* Success: Make, Ninja, Shake

### parallel: Parallelism

Given two targets, build them in parallel.

    parallel-run input1 -- output1; parallel-run input2 -- output2

* Success: Make, Ninja, Shake

### include: C #include files

Given a C file, compile it, automatically figuring out any transitively included dependencies. If any of those dependencies change in future, it should rebuild.

	gcc main.o -c include-main.c -o main.o

* Success: Make, Ninja, Shake

### wildcard: Build a file specified by an extension wildcard

Given a command line argument of `123.in`, copy `123.in` to `123.out`. Should work for any file with a `.in` suffix.

	cp $1 $1.out

* Success: Make, Shake
* **Ninja fails**: Ninja requires all rules to be listed in full.

### spaces: Build a file containing spaces

Work with files including spaces.

    cp "input file" "output file"

* Success: Ninja, Shake
* **Make fails**: Make does not support files with spaces in them.

### monad1: Monadic patterns

The monad series of tests are designed to probe the difference between applicative build systems and monadic ones, also showing which features allow applicative build systems to "fake" some monadic actions. The first requires depending on a list of files itself stored in a file.

    cat list | xargs cat > output

* Success: Make, Ninja, Shake

### monad2: More monadic patterns

The second test is like the first, but the `list` file itself is generated.

    monad2-run source -- list
    cat list | xargs cat > output

* Success: Make, Ninja, Shake

### monad3: More monadic patterns

The third test requires generating `list`, then generating the files `list` refers to.

    monad3-run source -- list
    monad3-gen -- gen                   # only if gen is in list
    cat list | xargs cat > output

* Success: Shake
* Unknown: Make, Ninja

### unchanged: Handle files which do not change

In some cases `input` will change, but `source` will not change in response. It is important that in these cases `output` is not regenerated.

    unchanged-gen input -- source
    unchanged-run source -- output

* Success: Ninja, Shake
* Unknown: Make

# Todo

These are tests that I would like to write, but have not yet done so.

### system: Dependency on system information

Introduce a dependency on a piece of system information that is not stored on the file system but must be recomputed every run.

### pool: Limit the parallelism in a specific stage

### generate: Add dependencies generated later

### multiple: Rules with multiple outputs 

### directory: Build all C files in a directory

Build all C files in a directory and link them together. Not yet implemented.
