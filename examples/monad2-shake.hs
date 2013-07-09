import Development.Shake
import Development.Shake.FilePath

main = shakeArgs shakeOptions $ do
    "output" *> \out -> do
        src <- readFileLines "list"
        need src
        cmd Shell "cat" src ">" [out]

    "list" *> \out -> do
        need ["source"]
        cmd Shell "sh monad2-run source list"
