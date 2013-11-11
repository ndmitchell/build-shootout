import Development.Shake

main = shakeArgs shakeOptions $ do
    "output" *> \out -> do
        need ["source"]
        cmd "sh unchanged-run source -- output"
    "source" *> \out -> do
        need ["input"]
        cmd "sh unchanged-gen input -- source"
