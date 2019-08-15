import Development.Shake

main = shakeArgs shakeOptions $ do
    want ["output1","output2"]
    let run inp out = out %> \_ -> do
            need [inp]
            cmd "sh parallel-run" [inp] "--" [out]
    run "input1" "output1"
    run "input2" "output2"
