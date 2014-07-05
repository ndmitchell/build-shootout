import Development.Shake

main = shakeArgs shakeOptions $ do
    "output" *> \out -> do
        need ["source"]
        cmd "sh system1-run source -- " [out]
    "source" *> \out -> do
        alwaysRerun
        cmd "sh system1-gen -- " [out]
