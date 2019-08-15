import Development.Shake
import Development.Shake.FilePath

main = shakeArgs shakeOptions $ do
    "*.out" %> \out -> do
        copyFile' (out -<.> "in") out
