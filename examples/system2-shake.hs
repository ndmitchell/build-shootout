import Development.Shake

main = shakeArgs shakeOptions $ do
    want ["output"]
    "output" %> \out -> do
        getEnv "SYSTEM2_DATA" -- just for the dependency
        cmd "sh system2-run" "--" [out]
