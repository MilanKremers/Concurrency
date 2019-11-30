module Main where

import Control.Concurrent
import Control.Monad
import System.Environment
import System.IO
import Data.IORef
import Data.ByteString.Char8           ( ByteString )
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import Data.Word
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Crypto.Hash.SHA1


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let config = parseConfig args

  case cfgMode config of
    Count -> do
              counter <- newIORef 0
              bool <- newIORef True
              lock <- return $ IORefLock bool
              configCounter config counter lock
              threadDelay 10000
              res <- readIORef counter
              print res
    List -> do
      counter <- newIORef 0
      bool <- newIORef True
      lock <- return $ IORefLock bool
      configList config counter lock
      threadDelay 10000
    Search h -> do
      counter <- newIORef 0
      bool <- newIORef True
      lock <- return $ IORefLock bool
      configSearch config counter lock h
      threadDelay 10000

  -- Do stuff here
  --putStrLn $ "I'm in program mode " ++ show (cfgMode config) ++ " with lock " ++ show (cfgSync config) ++ ","
  --putStrLn $ "performing the " ++ show (cfgModulus config) ++ "-test with " ++ show (cfgThreads config) ++ " threads"
  --putStrLn $ "on the numbers " ++ show (cfgLower config) ++ " .. " ++ show (cfgUpper config) ++ "."


  --forkIO $ replicateM_ 100 (putChar 'A')
  --forkIO $ replicateM_ 100 (putChar 'B')

-- Parses the command line arguments
parseConfig :: [String] -> Config
parseConfig (sync' : b : e : m : threads : mode' : rest)
  = Config sync mode (read b) (read e) (read m) (read threads)
  where
    -- Synchronization method
    sync = case sync' of
      "ioref" -> SyncIORef
      "mvar" -> SyncMVar
      _ -> error "Illegal sync method"
    -- Program mode
    mode = case (mode', rest) of
      ("count", []) -> Count
      ("list", []) -> List
      ("search", [q]) -> Search $ readHash q
      _ -> error "Illegal mode or wrong number of arguments"
parseConfig _ = error "Wrong number of arguments"

data Sync = SyncMVar | SyncIORef deriving Show
data Mode = Count | List | Search ByteString deriving Show

data Config = Config { cfgSync :: !Sync, cfgMode :: !Mode, cfgLower :: !Int, cfgUpper :: !Int, cfgModulus :: !Int, cfgThreads :: !Int } deriving Show

-- Reads a hash passed as command line argument.
readHash :: String -> ByteString
readHash = B.pack . readHash'
 
-- Two hexadecimal characters become one byte. Input size must thus be even.
readHash' :: String -> [Word8]
readHash' [] = []
readHash' [_] = error "Illegal hexadecimal hash"
readHash' (c1:c2:cs) = v1 * 16 + v2 : readHash' cs
  where
    v1 = value c1
    v2 = value c2
    value c = fromIntegral $ fromMaybe (error "Illegal hexadecimal hash") $ readHexadecimal c

readHexadecimal :: Char -> Maybe Int
readHexadecimal c = c `elemIndex` (['0'..'9'] ++ ['a'..'f'])

-- Checks if a number matches with the specified hash
checkHash :: ByteString -> Int -> Bool
checkHash expected value = expected == hash (B8.pack $ show value)

-- Search
configSearch :: Config -> IORef Int -> Lock -> ByteString -> IO ()
configSearch Config {cfgLower = x, cfgUpper = y, cfgThreads = z, cfgModulus = m} c lock h = do
   startThreadsSearch (divider x y z) m c lock h
   return () 
  
startThreadsSearch :: [[Int]] -> Int -> IORef Int -> Lock -> ByteString -> IO () 
startThreadsSearch [x] m c lock h = do
     forkIO $ countThreadSearch x m c lock h
     return ()    
startThreadsSearch (x:xs) m c lock h = do 
     startThreadsSearch xs m c lock h
     forkIO $ countThreadSearch x m c lock h
     return ()
 
countThreadSearch :: [Int] -> Int -> IORef Int -> Lock -> ByteString -> IO ()
countThreadSearch [x] m c l h| mTest x m && checkHash h x = do 
                                    interlocked l (putStrLn(show x))
                                    return () 
                             | otherwise = do
                               interlocked l (putStrLn ("not found"))
                               return ()
                         
countThreadsearch (x:xs) m c l h| mTest x m && checkHash h x = do 
                                    interlocked l (putStrLn(show x))
                               | otherwise =
                                    countThreadSearch xs m c l h

data Lock = IORefLock (IORef Bool)

-- Guard some action using the given lock

interlocked :: Lock -> IO a -> IO a
interlocked l@(IORefLock lock) action = do
  unlocked <- atomicModifyIORef' lock $ \old -> 
      if old == True
        then (False, True)
        else (False, False)
  if unlocked 
    then do
      res <- action
      writeIORef lock True
      return res
    else interlocked l action 

--mTest
mTest :: Int -> Int -> Bool
mTest b m = check (sum(help (length (list b)) (list b))) m

list :: Int -> [Int]
list 0 = []
list b = list (b `div` 10) ++ [b `mod` 10]

help :: Int -> [Int] -> [Int]
help _ [] = []
help a (x:xs) = [a*x] ++ help (a-1) xs

check :: Int -> Int -> Bool
check _ 0 = False
check _ 1 = True
check x y | x `mod` y == 0 = True
          | otherwise = False

-- divider schrijven
divider :: Int -> Int -> Int -> [[Int]]
divider min max t = divs (devide (max - min) t) min

devide :: Int -> Int -> [Int]
devide n 1 = [n]
devide n t = devides n t (n `mod` t) t

devides :: Int -> Int -> Int -> Int -> [Int]
devides n t m 1 = [n `div` t]
devides n t m x | m > 0 = ((n `div` t) + 1) : devides n t (m-1) (x-1)
                | otherwise = (n `div` t) : devides n t m (x-1)

divs :: [Int] -> Int -> [[Int]]
divs [] _ = [[]]
divs [x] min = [iets x min]
divs (x:xs) min = [iets x min] ++ divs xs (min + x)

iets :: Int -> Int -> [Int]
iets x min | x == 1 = [min]
           | otherwise = min : iets (x-1) (min + 1)

--Count
configCounter :: Config -> IORef Int -> Lock -> IO ()
configCounter Config {cfgLower = x, cfgUpper = y, cfgThreads = z, cfgModulus = m} c lock = do
   startThreads (divider x y z) m c lock
   return () 
 
 
startThreads :: [[Int]] -> Int -> IORef Int -> Lock -> IO () 
startThreads [x] m c lock = do
     forkIO $ countThread x m c lock
     return ()    
startThreads (x:xs) m c lock = do 
     startThreads xs m c lock
     forkIO $ countThread x m c lock
     return ()
 
countThread :: [Int] -> Int -> IORef Int -> Lock -> IO ()
countThread [x] m c l| mTest x m = do 
                             interlocked l (modifyIORef c (\a -> a + 1))
                             return () 
                      | otherwise = return ()
                         
countThread (x:xs) m c l | mTest x m = do 
                              interlocked l (modifyIORef c (\a -> a + 1))
                              countThread xs m c l
                         | otherwise =
                             countThread xs m c l
 
--List
configList :: Config -> IORef Int -> Lock -> IO ()
configList Config {cfgLower = x, cfgUpper = y, cfgThreads = z, cfgModulus = m} c lock = do
   startThreadsList (divider x y z) m c lock
   return () 
  
startThreadsList :: [[Int]] -> Int -> IORef Int -> Lock -> IO () 
startThreadsList [x] m c lock = do
     forkIO $ countThreadList x m c lock
     return ()    
startThreadsList (x:xs) m c lock = do 
     startThreadsList xs m c lock
     forkIO $ countThreadList x m c lock
     return ()
 
countThreadList :: [Int] -> Int -> IORef Int -> Lock -> IO ()
countThreadList [x] m c l| mTest x m = do 
                             interlocked l (modifyIORef c (\a -> a + 1))
                             counter <- readIORef c
                             interlocked l (putStrLn( show counter ++ " " ++ show x ))
                             return () 
                      | otherwise = return ()
                         
countThreadList (x:xs) m c l | mTest x m = do 
                              interlocked l (modifyIORef c (\a -> a + 1))
                              counter <- readIORef c
                              interlocked l (putStrLn( show counter ++ " " ++ show x ))
                              countThreadList xs m c l
                         | otherwise =
                             countThreadList xs m c l


