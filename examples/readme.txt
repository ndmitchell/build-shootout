Examples for "Build System Shootout" project.
Site: https://github.com/ndmitchell/build-shootout

This project attempts to clarify the relative power of various build systems.
It attempts to answer whether a build system is capable of expressing a particular dependency structure,
but does not measure performance.

These examples can be run only as part build-shootout test run.
They can't be run from this directory.

Descriptions of each test can be found on the build-shootout page.

Below are the tests results comparing with SCons, GNU Make and Ninja:

+-----------------+------+-------+-------+---------+
|                 | Make | Ninja | Scons | Aqualid |
+-----------------+------+-------+-------+---------+
| basic           |  yes |  yes  |  yes  |   yes   |
| parallel        |  yes |  yes  |  yes  |   yes   |
| include         |  yes |  yes  |  yes  |   yes   |
| wildcard        |  yes |  no   |  yes* |   yes*  |
| spaces          |  yes |  yes  |  yes  |   yes   |
| monad1          |  yes |  yes  |  yes  |   yes   |
| monad2          |  yes |  yes  |  yes  |   yes   |
| monad3          |  yes |   no  |  yes  |   yes*  |
| unchanged       |   no |  yes  |  yes  |   yes   |
| multiple        |   no |  yes  |  yes  |   yes   |
| system1         |   no |   no  |  yes  |   yes   |
| system2         |   no |   no  |  yes  |   yes   |
| pool            |   no |  yes  |   no  |    no   |
| digest          |   no |   no  |  yes  |   yes   |
| nofileout       |   no |   no  |   no  |   yes   |
| noleftover      |   no |   no  |   no  |    no   |
| secondary       |  yes |   no  |   no  |    no   |
| intermediate    |  yes |   no  |   no  |    no   |
+-----------------+------+-------+-------+---------+


$ runhaskell Main aql
## basic aql ... Success
## parallel aql ... Success
## include aql ... Success
## wildcard aql ... Success
## spaces aql ... Success
## monad1 aql ... Success
## monad2 aql ... Success
## monad3 aql ... Success
## unchanged aql ... Success
## multiple aql ... Success
## system1 aql ... Success
## system2 aql ... Success
## digest aql ... Success
## nofileout aql ... Success
