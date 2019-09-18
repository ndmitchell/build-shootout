import Development.Shake

main = shakeArgs shakeOptions $ do
    let run inp out = out %> \_ -> do
            need [inp]
            cmd "sh multiple-run" [inp] "--" [out]
    run "source1" "output1"
    run "source2" "output2"
    ["source1","source2"] |%> \out -> do
        need ["input"]
        cmd "sh multiple-gen input -- source1 source2"
