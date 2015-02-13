
import Development.Shake

main = shakeArgs shakeOptions $ do
    want ["output"]
    "output" *> \out -> do
        need ["input"]
        orderOnly ["secondary"]
        cmd "sh secondary-run secondary -- output"
    "secondary" *> \out -> do
        need ["input"]
        cmd "sh secondary-run input -- secondary"
