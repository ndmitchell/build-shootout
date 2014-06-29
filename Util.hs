{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}

-- | A test script to check those build systems claiming to implement a test
--   do in fact do so.
module Util(
    test, Opt(..),
    randomRIO,
    writeBinary
    ) where

import Control.Concurrent
import Control.Exception as E
import Control.Monad
import Data.Char
import Data.List
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Info
import System.Random


data Opt
    = NoChange
    | Contents FilePath String
    | Missing FilePath
    | Change FilePath
    | Parallel Int
    | Target FilePath
    | Log String
      deriving Show


data Tool = Tup | TupLua | Ninja | Shake | Make | Fabricate
    deriving (Show,Eq,Enum,Bounded)


writeBinary :: FilePath -> String -> IO ()
writeBinary file out = withBinaryFile file WriteMode $ \h -> hPutStr h out

filterArgs :: IO ([String], String -> Bool, Tool -> Bool)
filterArgs = do
    xs <- getArgs
    (args, xs) <- return $ partition ("-" `isPrefixOf`) xs
    let ts = [lcase $ show t| t :: Tool <- [minBound..maxBound]]
    let (tool,norm) = partition (`elem` ts) $ map lcase xs
    return (map (dropWhile (== '-')) args
           ,(\x -> null norm || lcase x `elem` norm)
           ,(\x -> null tool || lcase (show x) `elem` tool))


lcase :: String -> String
lcase = map toLower


test :: String -> (([Opt] -> IO ()) -> IO ()) -> IO ()
test name f = do
    (options, filtName, filtTool) <- filterArgs
    hSetBuffering stdout NoBuffering
    ts <- findTools name
    forM_ ts $ \t -> when (filtName name && filtTool t) $ do
        putStr $ "## " ++ name ++ " " ++ lcase (show t) ++ " ... "
        killDir "temp"
        createDir "temp"
        forM_ ["examples","util"] $ \dir -> do
            xs <- getDirectoryContents dir
            sequence_ [copyFile (dir </> x) ("temp" </> x) | x <- xs, (name ++ "-") `isPrefixOf` x]
        writeFile ".log" ""
        withCurrentDirectory "temp" $
            (if "continue" `elem` options then continue else id) $
                f $ run name t
        killDir "temp" -- deliberately don't clean up on failure
        removeFile ".log"
        putStrLn $ "Success"


continue :: IO () -> IO ()
continue act = E.catch act $ \(e :: SomeException) -> print e >> putStrLn "<FAILURE>"



killDir :: FilePath -> IO ()
killDir x = retryIO (fmap not $ doesDirectoryExist x) $ removeDirectoryRecursive x

createDir :: FilePath -> IO ()
createDir x = retryIO (doesDirectoryExist x) $ createDirectoryIfMissing True x

retryIO :: IO Bool -> IO () -> IO ()
retryIO test act = f 20
    where
        pause = threadDelay 100000
        f 0 = act >> pause
        f i = do b <- test
                 unless b $ do
                    E.catch act $ \(_ :: SomeException) -> return ()
                    pause
                    f (i-1)


findTools :: String -> IO [Tool]
findTools name = do
    xs <- getDirectoryContents "examples"
    let ts = [v | x <- xs, takeExtension x /= ".broken", Just v <- [stripPrefix (name ++ "-") $ dropExtensions x]]
    return [x | x <- [minBound..maxBound], map toLower (show x) `elem` ts]


run :: String -> Tool -> [Opt] -> IO ()
run name tool opts = do
    verbose <- fmap ("--verbose" `elem`) getArgs
    when verbose $ putStrLn $ "\n" ++ name ++ " " ++ show tool ++ " " ++ show opts
    xs <- mapM (opt tool) opts
    threadDelay 1000000
    let p = last $ 1 : [i | Parallel i <- opts]
    let target = unwords ["\"" ++ x ++ "\"" | Target x <- opts]
    case tool of
        Shake -> system_ $ "runhaskell -Werror -fwarn-unused-binds -fwarn-unused-imports " ++ name ++ "-shake.hs --quiet -j" ++ show p ++ " " ++ target
        Make -> system_ $ "make --file=" ++ name ++ "-make --quiet -j" ++ show p ++ " " ++ target
        Ninja -> system_ $ "ninja -f " ++ name ++ "-ninja.ninja -j" ++ show p ++ " " ++ target ++ " > " ++ devNull
        Tup -> do
            b <- doesDirectoryExist ".tup"
            unless b $ system_ $ "tup init > " ++ devNull
            writeFile ".tup/options" "[updater]\nwarnings = 0"
            copyFile (name ++ "-tup")  "Tupfile"
            system_ $ "tup -j" ++ show p ++ " " ++ target ++ " > " ++ devNull
            removeFile "Tupfile"
        TupLua -> do
            b <- doesDirectoryExist ".tup"
            unless b $ system_ $ "tup init > " ++ devNull
            writeFile ".tup/options" "[updater]\nwarnings = 0"
            copyFile (name ++ "-tuplua")  "Tupfile.lua"
            system_ $ "tup -j" ++ show p ++ " " ++ target ++ " > " ++ devNull
            removeFile "Tupfile.lua"
        Fabricate -> do
            system_ $ "python " ++ name ++ "-fabricate.py" ++ (if verbose then "" else " --quiet") ++ " -j" ++ show p ++ " " ++ target
            when verbose $ putStrLn =<< readFile ".deps"
    sequence_ xs

windows :: Bool
windows = os == "mingw32"

devNull :: String
devNull = if windows then "nul" else "/dev/null"


opt :: Tool -> Opt -> IO (IO ())
opt _ Parallel{} = return $ return ()
opt _ Target{} = return $ return ()
opt _ (Missing file) = return $ do
    b <- doesFileExist file
    when b $ error $ "Fail should be missing: " ++ file
opt _ (Contents file x) = return $ do
    src <- readFile file
    when (src /= x) $ error $
        "File is wrong: " ++ file ++ "\n" ++
        "Expected: " ++ show x ++ "\n" ++
        "Got     : " ++ show src
opt _ (Log x) = return $ do
    src <- readFile "../.log"
    when (lines src /= words x) $ error $
        "File is wrong: ../.log\n" ++
        "Expected: " ++ show x ++ "\n" ++
        "Got     : " ++ show (unwords $ words src)
opt _ NoChange = do
    dir <- getDirectoryContents "."
    times <- mapM modTime dir
    return $ do
        dir2 <- getDirectoryContents "."
        same "List of files changed" dir dir2
        times2 <- mapM modTime dir
        same "File was modified" (zip dir times) (zip dir2 times2)
opt _ (Change file) = do
    a <- modTime file
    return $ do
        b <- modTime file
        when (a == b) $ error "File did not change"


same :: (Show a, Eq a) => String -> [a] -> [a] -> IO ()
same msg a b | null add && null del = return ()
             | otherwise = error $ msg ++ "\nOld values: " ++ show del ++ "\nNew values: " ++ show add
    where
        add = b \\ a
        del = a \\ b


modTime :: FilePath -> IO (Maybe String)
modTime file | "." `isPrefixOf` file = return Nothing
             | otherwise = do
                b <- doesFileExist file
                if b then fmap (Just . show) $ getModificationTime file else return Nothing


withCurrentDirectory :: FilePath -> IO () -> IO ()
withCurrentDirectory dir act = do
    curdir <- getCurrentDirectory
    bracket_ (setCurrentDirectory dir) (setCurrentDirectory curdir) act


system_ :: String -> IO ()
system_ cmd = do
    r <- system cmd
    when (r /= ExitSuccess) $ error $ "System command failed: " ++ cmd
