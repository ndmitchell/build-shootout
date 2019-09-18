import Development.Shake
import Development.Shake.Util

main = shakeArgs shakeOptions $ do
    want ["main.o"]
    "main.o" %> \out -> do
        () <- cmd "gcc -MMD -MT main.o -MF main.d -c include-main.c -o main.o"
        needMakefileDependencies "main.d"
