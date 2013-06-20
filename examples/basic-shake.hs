import Development.Shake
import Development.Shake.Command

main = shakeArgs shakeOptions $ do
    want ["output"]
    "output" *> \_ -> do
        need ["input"]
        cmd "sh basic-create input output"
