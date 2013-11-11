import Development.Shake

main = shakeArgs shakeOptions $ do
    "output*" *> \out -> do
        let src = "source" ++ [last out]
        need [src]
        cmd "sh multiple-run" [src] " -- " [out]
    ["source1","source2"] **> \out -> do
        need ["input"]
        cmd "sh multiple-gen input -- source1 source2"
