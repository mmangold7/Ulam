module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Vector (Vector, (!), freeze)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import System.Environment (getArgs)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Control.Monad (when)
import Codec.Picture

pixelSize :: Int
pixelSize = 1

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
            putStrLn $ "Generated primes up to " ++ show maxNum
            createUlamSpiralImage maxNum primes
        ["2", nthStr] -> do
            let nth = read nthStr :: Int
            putStrLn $ "Mode 2: Finding the " ++ show nth ++ "th prime"
            nthPrime <- findNthPrime nth
            putStrLn $ "The " ++ show nth ++ "th prime is " ++ show nthPrime
            let maxNum = estimateLimit nth
            primes <- generatePrimes maxNum
            createUlamSpiralImage maxNum primes
        _ -> putStrLn "Usage: program 1 <max_num> | program 2 <nth>"

generatePrimes :: Int -> IO (Vector Bool)
generatePrimes limit = do
    putStrLn "Initializing vector..."
    mvec <- M.replicate (limit + 1) True
    M.write mvec 0 False
    M.write mvec 1 False
    putStrLn "Starting sieve..."
    let totalSteps = floor (sqrt (fromIntegral limit) :: Double)
    mapM_ (markNonPrimes mvec totalSteps) [2..totalSteps]
    putStrLn "Sieve completed, freezing vector..."
    freeze mvec
  where
    markNonPrimes :: M.IOVector Bool -> Int -> Int -> IO ()
    markNonPrimes vec totalSteps n = do
        let len = M.length vec
        when (n `mod` (totalSteps `div` 100) == 0) $ do
            putStrLn $ "Progress: " ++ show (n * 100 `div` totalSteps) ++ "%"
        if n * n < len
            then do
                let indices = [n * n, n * n + n .. len - 1]
                let chunks = parMap rdeepseq id (chunkList 1000 indices)
                mapM_ (mapM_ (\i -> M.write vec i False)) chunks
            else return ()

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = let (ys, zs) = splitAt n xs in ys : chunkList n zs

findNthPrime :: Int -> IO Int
findNthPrime nth = do
    let initialLimit = estimateLimit nth
    findNthPrimeHelper nth initialLimit 0 initialLimit

-- from prime number theorem
estimateLimit :: Int -> Int
estimateLimit n = ceiling (fromIntegral n * log (fromIntegral n * log (fromIntegral n)))

findNthPrimeHelper :: Int -> Int -> Int -> Int -> IO Int
findNthPrimeHelper nth limit totalWork initialLimit = do
    let progress = (totalWork * 100) `div` initialLimit
    putStrLn $ "Overall Progress: " ++ show progress ++ "%"
    primes <- generatePrimes limit
    let primeIndices = V.findIndices id primes
    if V.length primeIndices >= nth
        then return (primeIndices ! (nth - 1))
        else findNthPrimeHelper nth (limit * 2) (totalWork + limit) initialLimit

createUlamSpiralImage :: Int -> Vector Bool -> IO ()
createUlamSpiralImage maxNum primes = do
    let size = ceiling (sqrt (fromIntegral maxNum)) :: Int
        img = createPrimeImage (size * pixelSize) (size * pixelSize) primes
    putStrLn $ "Image size: " ++ show (size * pixelSize) ++ "x" ++ show (size * pixelSize)
    putStrLn "Saving image..."
    savePngImage "ulam_spiral.png" (ImageY8 img)
    putStrLn "Ulam spiral image created: ulam_spiral.png"

createPrimeImage :: Int -> Int -> Vector Bool -> Image Pixel8
createPrimeImage width height primes = generateImage pixelFunc width height
  where
    centerX = width `div` (2 * pixelSize)
    centerY = height `div` (2 * pixelSize)
    pixelFunc :: Int -> Int -> Pixel8
    pixelFunc x y =
      let
        coordX = (x `div` pixelSize) - centerX
        coordY = - (y `div` pixelSize) + centerY
        num = coordToNum coordX coordY
      in if num > 0 && num < V.length primes && primes ! num then 0 else 255

coordToNum :: Int -> Int -> Int
coordToNum x y = 
    let k = max (abs x) (abs y)
        base = (2 * k - 1) ^ 2
    in case (x, y) of
        (x', y') | x' == k  -> base + (k + y')
        (x', y') | y' == k  -> base + 2 * k + (k - x')
        (x', y') | x' == -k -> base + 4 * k + (k - y')
        (x', y') | y' == -k -> base + 6 * k + (k + x')
        _                   -> 0

data Dir = R | U | L | D deriving (Enum, Show)

spiralSeq :: Int -> [Dir]
spiralSeq n = replicate n R ++ replicate n U ++ replicate (n + 1) L ++ replicate (n + 1) D

spiral :: [Dir]
spiral = concatMap spiralSeq [1, 3..]

move :: (Int, Int) -> Dir -> (Int, Int)
move (x, y) R = (x + 1, y)
move (x, y) U = (x, y + 1)
move (x, y) L = (x - 1, y)
move (x, y) D = (x, y - 1)

spiralPos :: [(Int, Int)]
spiralPos = scanl move (0, 0) spiral

mainEntry :: IO ()
mainEntry = main
