import Development.Shake

main = shakeArgs shakeOptions{shakeChange=ChangeDigest} $ do
    want ["output"]
    "output" *> \_ -> do
        need ["input"]
        cmd "sh digest-run input -- output"
