{-# LANGUAGE ScopedTypeVariables #-}

-- | A test script to check those build systems claiming to implement a test
--   do in fact do so.
module Util(test, Opt(..), Tool(..)) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath
import System.IO


data Opt
    = NoChange
    | Contents FilePath String
    | Parallel Int


data Tool = Ninja | Shake | Make
    deriving (Show,Eq,Enum,Bounded)


test :: String -> (([Opt] -> IO ()) -> IO ()) -> IO ()
test name f = do
    hSetBuffering stdout NoBuffering
    ts <- findTools name
    forM_ ts $ \t -> do
        putStr $ "## Testing " ++ name ++ " " ++ show t ++ "... "
        let clean = catch (removeDirectoryRecursive "temp") $ \(_ :: SomeException) -> return ()
        clean
        createDirectoryIfMissing True "temp"
        forM_ ["examples","util"] $ \dir -> do
            xs <- getDirectoryContents dir
            sequence_ [copyFile (dir </> x) ("temp" </> x) | x <- xs, (name ++ "-") `isPrefixOf` x]
        withCurrentDirectory "temp" $ do
            f $ run name t
        clean -- deliberately don't clean up on failure
        putStrLn $ "Success"


findTools :: String -> IO [Tool]
findTools name = do
    xs <- getDirectoryContents "examples"
    let ts = [v | x <- xs, Just v <- [stripPrefix (name ++ "-") $ dropExtensions x]]
    return [x | x <- [minBound..maxBound], map toLower (show x) `elem` ts]


run :: String -> Tool -> [Opt] -> IO ()
run name tool opts = do
    xs <- mapM (opt tool) opts
    threadDelay 1000000
    let p = last $ 1 : [i | Parallel i <- opts]
    case tool of
        Shake -> system_ $ "runhaskell " ++ name ++ "-shake.hs --quiet -j" ++ show p
        Make -> system_ $ "make --file=" ++ name ++ "-make --quiet -j" ++ show p
        Ninja -> system_ $ "sh -c \"ninja -f " ++ name ++ "-ninja.ninja -j" ++ show p ++ " > /dev/null\""
    sequence_ xs


opt :: Tool -> Opt -> IO (IO ())
opt _ (Contents file x) = return $ do
    src <- readFile file
    when (src /= x) $ error $ "File is wrong: " ++ file
opt _ NoChange = do
    dir <- getDirectoryContents "."
    times <- mapM modTime dir
    return $ do
        dir2 <- getDirectoryContents "."
        when (dir /= dir2) $ error "Contents changed"
        times2 <- mapM modTime dir
        when (times /= times2) $ error "Files were modified"
opt _ Parallel{} = return $ return ()


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
    when (r /= ExitSuccess) $ error "System command failed"
