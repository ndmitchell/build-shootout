import Development.Shake

main = shakeArgs shakeOptions $ do

    want $ ["output"]

    "output" *> \out -> do
        maybeVal <- getEnv "SYSTEM2_DATA"
        cmd "sh system2-run" "--" [out]
