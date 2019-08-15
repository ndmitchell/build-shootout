import Development.Shake

main = shakeArgs shakeOptions $ do
    "output file" %> \out -> do
        copyFile' "input file" out
