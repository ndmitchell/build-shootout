import Development.Shake
import Development.Shake.Command
import Data.List


main = shakeArgs shakeOptions $ do
    want ["main.o"]
    "main.o" *> \out -> do
        () <- cmd "gcc -MMD -MT main.o -MF main.d -c include-main.c -o main.o"
        src <- liftIO $ readFile "main.d"
        need $ concatMap snd $ parseMakefile src


parseMakefile :: String -> [(FilePath, [FilePath])]
parseMakefile = concatMap f . join . lines
    where
        join (x1:x2:xs) | "\\" `isSuffixOf` x1 = join $ (init x1 ++ x2) : xs
        join (x:xs) = x : join xs
        join [] = []

        f x = [(a, words $ drop 1 b) | a <- words a]
            where (a,b) = break (== ':') $ takeWhile (/= '#') x
