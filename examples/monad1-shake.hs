import Development.Shake
import Development.Shake.FilePath

main = shakeArgs shakeOptions $ do
    "output" *> \out -> do
        src <- readFileLines "list"
        need src
        cmd Shell "cat" src ">" [out]
