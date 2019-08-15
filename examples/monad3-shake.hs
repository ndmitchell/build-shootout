import Development.Shake

main = shakeArgs shakeOptions $ do
    "output" %> \out -> do
        src <- readFileLines "list"
        need src
        cmd Shell "cat" src ">" [out]

    "list" %> \out -> do
        need ["source"]
        cmd Shell "sh monad3-run source -- list"

    "gen" %> \out -> do
        cmd Shell "sh monad3-gen -- gen"
