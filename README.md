# Build System Shootout

This project attempts to clarify the relative power of various build systems. Compared to the [Computer Language Shootout](http://benchmarksgame.alioth.debian.org/), this Shootout attempts to answer whether a build system is capable of expressing a particular dependency structure, but does not measure performance. The following build systems have at least one entry:

* [Shake](https://github.com/ndmitchell/shake#readme), cross-platform
* [Ninja](http://martine.github.io/ninja/), cross-platform
* [GNU Make](http://www.gnu.org/software/make/), cross-platform

Below are a list of tests, a description of the test, and a list of build systems that can implement the test, and a list of those that are currently believed to lack the power to implement the test.

To pass a test the build system must follow the specification, must not rebuild anything in subsequent builds (all files must end up clean) and must not require explicit assume dirty/assume clean flags to be specified. If a build system requires restarting (and rechecking all previously checked dependency files) it is considered a partial pass.

Performance is deliberately not measured as all actions are specified via shell scripts to make the results as similar as possible - even if some of the build systems would not use that approach.

### basic: Basic dependency

Given an input file, create an output file which is a copy of the input file. If the input file changes, or the output file is not present, the output file should be created.

### parallel: Parallelism

Given two targets, build them in parallel.

### include: C #include files

Given a C file, compile it, automatically figuring out any transitively included dependencies. If any of those dependencies change in future, it should rebuild.

### wildcard: Build a file specified by an extension wildcard

### directory: Build all C files in a directory



### system: Dependency on system information

Introduce a dependency on a piece of system information that is not stored on the file system but must be recomputed every run.

### pool: Limit the parallelism in a specific stage

### generate: Add dependencies generated later

### unchanged: Handle files which do not change

### multiple: Rules with multiple outputs 

### spaces: Can you build files containing spaces

