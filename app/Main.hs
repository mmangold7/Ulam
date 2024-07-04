module Main where

import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.Vector (Vector, (!), generate, thaw, freeze)
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import System.Environment (getArgs)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Control.Monad (when)
import System.IO (hFlush, stdout)

-- Main function
main :: IO ()
main = do
    putStrLn "Hello, World!"
    args <- getArgs
    numCores <- getNumProcessors
    setNumCapabilities numCores
    putStrLn $ "Using " ++ show numCores ++ " cores"
    case args of
        ["1", maxStr] -> do
            let maxNum = read maxStr :: Int
            putStrLn $ "Mode 1: Finding primes up to " ++ show maxNum
            primes <- generatePrimes maxNum
            putStrLn $ "Generated " ++ show (V.length primes) ++ " primes up to " ++ show maxNum
        ["2", nthStr] -> do
            let nth = read nthStr :: Int
            putStrLn $ "Mode 2: Finding the " ++ show nth ++ "th prime"
            nthPrime <- findNthPrime nth
            putStrLn $ "The " ++ show nth ++ "th prime is " ++ show nthPrime
        _ -> putStrLn "Usage: program 1 <max_num> | program 2 <nth>"

-- Generate prime numbers up to a given limit using the Sieve of Eratosthenes
generatePrimes :: Int -> IO (Vector Int)
generatePrimes limit = do
    putStrLn "Initializing vector..."
    mvec <- thaw initialVector
    putStrLn "Starting sieve..."
    let totalSteps = floor (sqrt (fromIntegral limit) :: Double)
    mapM_ (markNonPrimesWithProgress mvec totalSteps) [2..totalSteps]
    putStrLn "Sieve completed, freezing vector..."
    frozenVec <- freeze mvec
    let primes = efficientFilter frozenVec
    putStrLn "Printing every thousandth prime..."
    printEveryThousandthPrime primes
    return primes
  where
    initialVector = generate (limit + 1) id

-- Mark non-primes in the mutable vector with progress tracking
markNonPrimesWithProgress :: M.IOVector Int -> Int -> Int -> IO ()
markNonPrimesWithProgress vec totalSteps n = do
    let len = M.length vec
    when (n `mod` 100 == 0) $ putStrLn $ "Progress: " ++ show (n * 100 `div` totalSteps) ++ "%"
    hFlush stdout  -- Ensure immediate output
    if n * n < len
        then do
            let indices = [n * n, n * n + n .. len - 1]
            mapM_ (\i -> M.write vec i 0) indices
        else return ()

-- Parallelize the marking of non-primes
parMarkNonPrimes :: M.IOVector Int -> Int -> Int -> [(Int, Int)]
parMarkNonPrimes _ n len =
    let indices = [n * n, n * n + n .. len - 1]
        updates = map (\i -> (i, 0)) indices
    in updates `using` parListChunk 1000 rdeepseq

-- Efficiently filter prime numbers
efficientFilter :: Vector Int -> Vector Int
efficientFilter vec = V.ifilter (\i x -> i > 1 && x /= 0) vec

-- Print every thousandth prime
printEveryThousandthPrime :: Vector Int -> IO ()
printEveryThousandthPrime primes = do
    let indexedPrimes = V.indexed primes
    mapM_ (\(i, p) -> when (i `mod` 1000 == 0) $ putStrLn ("Thousandth Prime #" ++ show (i `div` 1000) ++ ": " ++ show p)) indexedPrimes

-- Find the nth prime number
findNthPrime :: Int -> IO Int
findNthPrime nth = do
    let initialLimit = 1000000  -- Initial estimate for the limit
    primes <- generatePrimes initialLimit
    if V.length primes >= nth
        then return (primes ! (nth - 1))
        else findNthPrime (initialLimit * 2)  -- Double the limit and try again