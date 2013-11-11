import Development.Shake

main = shakeArgs shakeOptions $ do
    "output" *> \out -> do
        need ["source"]
        cmd "sh system-run source -- " [out]
    "source" *> \out -> do
        alwaysRerun
        cmd "sh system-gen -- " [out]
