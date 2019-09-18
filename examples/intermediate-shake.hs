
import Development.Shake

main = shakeArgs shakeOptions $ do
    want ["output"]
    "output" %> \out -> do
        need ["input"]
        orderOnly ["intermediate"]
        cmd "sh intermediate-run intermediate -- output"
    "intermediate" %> \out -> do
        need ["input"]
        removeFilesAfter "." ["intermediate"]
        cmd "sh intermediate-run input -- intermediate"
