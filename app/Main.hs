module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Vector (Vector, (!), generate, thaw, freeze)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import System.Environment (getArgs)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Control.Monad (when)
import System.IO (hFlush, stdout)
import Codec.Picture -- For image creation
import Codec.Picture.Types -- For image manipulation

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
            createUlamSpiralImage maxNum primes
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
    mapM_ (markNonPrimes mvec totalSteps) [2..totalSteps]
    putStrLn "Sieve completed, freezing vector..."
    frozenVec <- freeze mvec
    let primes = efficientFilter frozenVec
    putStrLn "Printing every thousandth prime..."
    printEveryThousandthPrime primes
    return primes
  where
    initialVector = generate (limit + 1) id

-- Mark non-primes in the mutable vector with progress tracking
markNonPrimes :: M.IOVector Int -> Int -> Int -> IO ()
markNonPrimes vec totalSteps n = do
    let len = M.length vec
    when (n `mod` (totalSteps `div` 100) == 0) $ do
        putStrLn $ "Progress: " ++ show (n * 100 `div` totalSteps) ++ "%"
    if n * n < len
        then do
            let indices = [n * n, n * n + n .. len - 1]
            let chunks = parMap rdeepseq id (chunkList 1000 indices)
            mapM_ (mapM_ (\i -> M.write vec i 0)) chunks
        else return ()

-- Helper function to chunk a list
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = let (ys, zs) = splitAt n xs in ys : chunkList n zs

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
    let initialLimit = estimateLimit nth  -- Estimate initial limit based on nth prime
    findNthPrimeHelper nth initialLimit 0 initialLimit

-- Estimate the upper bound for nth prime using the prime number theorem
estimateLimit :: Int -> Int
estimateLimit n = ceiling (fromIntegral n * log (fromIntegral n * log (fromIntegral n)))

-- Find the nth prime number with progress tracking
findNthPrimeHelper :: Int -> Int -> Int -> Int -> IO Int
findNthPrimeHelper nth limit totalWork initialLimit = do
    let progress = (totalWork * 100) `div` initialLimit
    putStrLn $ "Overall Progress: " ++ show progress ++ "%"
    primes <- generatePrimes limit
    if V.length primes >= nth
        then return (primes ! (nth - 1))
        else findNthPrimeHelper nth (limit * 2) (totalWork + limit) initialLimit

-- Create Ulam Spiral Image
createUlamSpiralImage :: Int -> Vector Int -> IO ()
createUlamSpiralImage maxNum primes = do
    let size = ceiling (sqrt (fromIntegral maxNum)) :: Int
        halfSize = size `div` 2
        img = createPrimeImage size size (halfSize, primes)
    putStrLn $ "Image size: " ++ show size ++ "x" ++ show size
    putStrLn "Saving image..."
    savePngImage "ulam_spiral.png" (ImageY8 img)
    putStrLn "Ulam spiral image created: ulam_spiral.png"

-- Create the image of the prime numbers in an Ulam spiral
createPrimeImage :: Int -> Int -> (Int, Vector Int) -> Image Pixel8
createPrimeImage width height (halfSize, primes) = generateImage pixelFunc width height
  where
    pixelFunc :: Int -> Int -> Pixel8
    pixelFunc x y = if V.elem (coordToNum (x - halfSize) (y - halfSize) width) primes then 0 else 255

-- Map coordinates to number in Ulam spiral
coordToNum :: Int -> Int -> Int -> Int
coordToNum x y size = 
    if x == 0 && y == 0 then 1
    else let
        layer = max (abs x) (abs y)
        layerMax = (2 * layer + 1) ^ 2
        offset = if x == layer && y > -layer then 3 * layer + y
                else if y == layer then layer - x
                else if x == -layer then 5 * layer - y
                else if y == -layer then 7 * layer + x
                else 0
    in layerMax - offset

-- Entry point
mainEntry :: IO ()
mainEntry = main
