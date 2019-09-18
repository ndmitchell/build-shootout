import Development.Shake

main = shakeArgs shakeOptions $ do
    want ["output1","output2","output3"]
    pool <- newResource "pool" 2
    let run inp out = out %> \_ -> do
            need [inp]
            withResource pool 1 $ cmd "sh pool-run" [inp] "--" [out]
    run "input1" "output1"
    run "input2" "output2"
    run "input3" "output3"
