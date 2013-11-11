import Development.Shake

main = shakeArgs shakeOptions $ do
    want ["output"]
    "output" *> \_ -> do
        need ["input"]
        cmd "sh basic-run input output"
