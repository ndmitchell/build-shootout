# Build System Shootout [![Build Status](https://travis-ci.org/ndmitchell/build-shootout.png)](https://travis-ci.org/ndmitchell/build-shootout)

This project attempts to clarify the relative power of various build systems. Compared to the [Computer Language Shootout](http://benchmarksgame.alioth.debian.org/), this Shootout attempts to answer whether a build system is capable of expressing a particular dependency structure, but does not measure performance. The following build systems have at least one entry:

* [Make](http://www.gnu.org/software/make/) (GNU version), cross-platform.
* [Ninja](http://martine.github.io/ninja/), cross-platform.
* [Shake](https://github.com/ndmitchell/shake#readme), cross-platform.
* [tup](http://gittup.org/tup/), cross-platform, requiring FUSE on Linux. Does not work with [Travis](https://travis-ci.org/) and cannot be compiled on Windows.
* [fabricate](https://code.google.com/p/fabricate/), works on Linux, some Windows support on some machines, requires at least admin configuration on Vista and above. Works partially with [Travis](https://travis-ci.org/).
* [SCons](http://www.scons.org/), cross-platform.
* [Aqualid](https://github.com/aqualid/), cross-platform.
* [Fbuild](https://github.com/felix-lang/fbuild), cross-platform.
* [Gup](https://github.com/timbertson/gup), cross-platform.

All build scripts are in the [examples directory](https://github.com/ndmitchell/build-shootout/tree/master/examples), as <tt><i>testname</i>-<i>buildsystem</i></tt>. You can run all the examples with `runhaskell Main` (after installing the [Haskell Platform](http://www.haskell.org/platform/), and any build systems you want to run). Use the argument `make` to only run Make examples, or `basic` to only run the basic test. 

Below are a list of tests, a description of the test, and how each build system fares on it. The tests have pseudo-code for the equivalent untracked straight-line shell script.

To pass a test the build system must:

* Follow the specification, including the test case. _Warning:_ this project is young, the specifications and tests are still evolving slightly.
* Must not rebuild things in subsequent runs, all files must end up clean.
* Must not require explicit assume dirty/assume clean flags to be specified.
* Must not explicitly check for the existence of a file (you can always write a build system in a line of shell for any of these problems, the idea is to use the build system).
* All build-shootout shell scripts must be treated as black boxes. any file listed before `--` is treated as an input, any file after is an output. Programs like `gcc`, `cat`, `xargs` and `cp` are the standard Posix utilities.

Performance is deliberately not measured as all actions are specified via shell scripts to make the results as similar as possible - even if some of the build systems would not normally use that approach.

#### Contributions

I welcome contributions, including:

* Examples in different build systems
* New implementations for existing build systems
* New test cases (provided they show something interesting)
* Corrections of my egregious errors

## Test cases

### basic: Basic dependency

Given an input file, create an output file which is a copy of the input file. If the input file changes, or the output file is not present, the output file should be created.

    basic-run input -- output

* **fabricate: success**
* **Make: success**
* **Ninja: success**
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* **tup: success**
* **Gup: success**

### parallel: Parallelism

Given two targets, build them in parallel.

    parallel-run input1 -- output1; parallel-run input2 -- output2

* fabricate: unimplemented, not tried
* **Make: success**
* **Ninja: success**
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* **tup: success**
* **Gup: success**

### include: C #include files

Given a C file, compile it, automatically figuring out any transitively included dependencies. If any of those dependencies change in future, it should rebuild.

    gcc main.o -c include-main.c -o main.o

* **fabricate: success**, but fails on Travis
* **Make: success**
* **Ninja: success**
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* **tup: success**
* **Gup: success**

### wildcard: Build a file specified by an extension wildcard

Given a command line argument of `123.in`, copy `123.in` to `123.out`. Should work for any file with a `.in` suffix.

    cp $1 $1.out

* **fabricate: success**
* **Make: success**
* Ninja: failure, requires all rules to be listed in full
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* **tup: success**
* **Gup: success**

### spaces: Build a file containing spaces

Work with files including spaces.

    cp "input file" "output file"

* fabricate: partial, generally works but requires custom code to get command line support for space-including targets
* **Make: success**
* **Ninja: success**
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* **tup: success**, seems to require Lua
* **Gup: success**

### monad1: Monadic patterns

The monad series of tests are designed to probe the difference between applicative build systems and monadic ones, also showing which features allow applicative build systems to "fake" some monadic actions. The first requires depending on a list of files itself stored in a file.

    cat list | xargs cat > output

* **fabricate: success**, but fails on Travis
* **Make: success**
* **Ninja: success**
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* **tup: success**
* **Gup: success**

### monad2: More monadic patterns

The second test is like the first, but the `list` file itself is generated.

    monad2-run source -- list
    cat list | xargs cat > output

* **fabricate: success**, but fails on Travis
* **Make: success**
* **Ninja: success**
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* **tup: success**
* **Gup: success**


### monad3: More monadic patterns

The third test requires generating `list`, then generating the files `list` refers to.

    monad3-run source -- list
    monad3-gen -- gen                   # only if gen is in list
    cat list | xargs cat > output

* **fabricate: success**, but fails on Travis
* **Make: success, requires automatic restarting**
* Ninja: unsure, no one has been able to implement it yet
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* tup: unsure, no one has been able to implement it yet
* **Gup: success**


### unchanged: Handle files which do not change

In some cases `input` will change, but `source` will not change in response. It is important that in these cases `output` is not regenerated.

    unchanged-gen input -- source
    unchanged-run source -- output

* **fabricate: success**, but fails on Travis
* Make: failure, does not seem to work
* **Ninja: success**, requires `restat` to be added
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* **tup: success**, requires `^o^` to be added
* **Gup: success**, requires explicitly calling `gup --contents`


### multiple: Rules with multiple outputs

In some cases one output will change, but not the other.

    multiple-gen input -- source1 source2
    multiple-run source1 -- output1
    multiple-run source2 -- output2

I believe this test can be written on top of `unchanged`, by encoding the dependencies appropriately.

* **fabricate: success**, but fails on Travis
* Make: failure, does not seem to work
* **Ninja: success**, requires `restat` to be added
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* **tup: success**, requires `^o^` to be added
* **Gup: success**, though hackish.


### system1: Dependency on system information

Introduce a dependency on a piece of system information that must be recomputed every run. In this scenario `system1-gen` might be equivalent to `gcc --version` and `system1-run` might be `gcc -c`. You must always test the `gcc` version, but only want to rebuild if it changes.

    system1-gen -- source          # must always be run
    system1-run source -- output   # must not run if source does not change

I believe that given a small amount of shell scripting glue (to run `system1-gen`) this test can be written on top of `unchanged`.

* fabricate: unsure
* Make: unsure
* Ninja: unsure
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**
* tup: unsure
* **Gup: success**


### system2: Dependency on system environment variable

Rerun if and only if `output` does not exist or system environment variable
`SYSTEM2_DATA` was changed.

    system2-run -- output

* fabricate: unsure
* Make: unsure
* Ninja: unsure
* SCons: unsure
* **Aqualid: success**
* **Shake: success**
* Fbuild: failure
* **tup: success**
* **Gup: success**


### pool: Limit the parallelism in a specific stage

Run with a parallelism of 8, but limit a specific stage to no more than 2 concurrent runs.

    pool-run input1 -- output1
    pool-run input2 -- output2
    pool-run input3 -- output3

* fabricate: unsure
* Make: failure, doesn't seem to work
* **Ninja: success**
* SCons: failure, doesn't support pools
* Aqualid: failure, doesn't support pools
* Fbuild: failure, doesn't support pools
* **Shake: success**
* tup: unsure, nothing I can see
* Gup: failure, doesn't support pools


### digest: Don't rebuild when a file is modified to the same value

The `input` file will be changed, but sometimes to the same value.

    digest-run input -- output

* fabricate: unsure
* Make: failure, doesn't support digests
* Ninja: failure, doesn't support digests
* **SCons: success**
* **Aqualid: success**
* **Fbuild: success**
* **Shake: success**, requires setting `Digest` change mode.
* tup: unsure
* **Gup: success**, requires explicitly calling `gup --contents`


### nofileout: Don't produce an output file

Rerun if and only if `input` file was changed.

    nofileout-run input --

* fabricate: unsure
* Make: unsure
* Ninja: unsure
* SCons: unsure
* **Aqualid: success**
* **Fbuild: success**
* Shake: failure, doesn't support rules that are only run if the dependencies change but don't produce an output file
* **tup: success**
* **Gup: success**, though a stamp file still needs to be created.


### noleftover: Remove files left over from a previous build

    # initialize
    echo "foo" > foo.in
    echo "bar" > bar.in
    # build
    noleftover-run foo.in -- foo.out
    noleftover-run bar.in -- bar.out
    # modify
    rm bar.in
    echo "baz" > baz.in
    # rebuld
    rm bar.out
    noleftover-run baz.in -- baz.out

* fabricate: unsure
* Make: unsure
* Ninja: unsure
* SCons: unsure
* Aqualid: failure
* Fbuild: failure
* Shake: failure, doesn't seem to be supported
* **tup: success**
* Gup: failure


### secondary: Secondary target

Building an `output` file from an `input` file requires an auxiliary result `secondary` file.
Removing `secondary` file doesn't cause rebuilding `output` file as long as `input` file wasn't changed.
Changing `input` file causes a rebuild of both files, `secondary` and `output`.

Within the scope of this test `change` means modification of both, contents and timestamp.

* fabricate: unsure
* **Make: success**
* Ninja: unsure
* SCons: unsure
* Aqualid: failure
* Fbuild: failure
* **Shake: success**
* tup: unsure
* Gup: failure


### intermediate: Intermediate target

Building an `output` file from an `input` file requires an auxiliary result `intermediate` file which is automatically removed at the end.
An `intermediate` file isn't created in subsequent builds as long as `input` file wasn't changed.
Changing `input` file causes building `intermediate` file, rebuilding `output` file, and removing `intermediate` file, eventually.

Within the scope of this test `change` means modification of both, contents and timestamp.

* fabricate: unsure
* **Make: success**
* Ninja: unsure
* SCons: unsure
* Aqualid: failure
* **Fbuild: success**
* **Shake: success**
* tup: unsure
* Gup: failure


## Build System Power

The intention of this project is to figure out what dependency features each build system offers, what power they give, and which features can be expressed in terms of others. This section is speculative and evolving.

### Pre dependencies (applicative) [all but Fabricate]

A pre dependency is one where you can introduce a dependency at the start, for example Make's `output: input`. Each output is allowed to express multiple dependencies, but they are all evaluated in isolation from each other.

### Post dependencies [Ninja, Fbuild, Shake, tup, Gup]

A post dependency is one where you introduce a dependency at the end, for example Ninja's `depfile`. These dependencies do not alter this build run, but will add dependencies for the next run.

### Mid dependencies (monadic) [Shake, Fbuild, Gup, subsumes pre and post dependencies]

A monadic dependency lets you introduce a new dependency while running an action after consulting previous dependencies, for example Shake's `need`.

### Auto post dependencies [tup, subsumes post dependencies]

An auto post dependency is one computed from what you actually used, rather than explicitly stated dependencies.

### Auto cached commands [fabricate, Fbuild]

A cached command is where the inputs/outputs for a command are tracked, and the command is treated as a pure function and skipped if its inputs didn't change. This feature is more useful in build systems that go forward (from inputs to outputs) rather than the standard build systems that go from outputs to inputs.

### Regenerate [Make]

Make lets you regenerate the Makefile and then continue again. How that works is anyones guess.
