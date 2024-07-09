module Main where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Vector (Vector, (!), freeze)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import System.Environment (getArgs)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Control.Monad (when)
import Codec.Picture
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)

pixelSize :: Int
pixelSize = 1

main :: IO ()
main = do
    startTime <- getCurrentTime
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
        ["3", inputFile] -> do
            putStrLn $ "Mode 3: Analyzing and highlighting the Ulam spiral from " ++ inputFile
            analyzeAndHighlightImage inputFile "ulam_highlighted.png"
        ["4", inputFile] -> do
            putStrLn $ "Mode 4: Visualizing diagonals with gradient from " ++ inputFile
            visualizeDiagonalsGradient inputFile "ulam_diagonals_gradient.png"
        ["5", inputFile] -> do
            putStrLn $ "Mode 5: Visualizing density with gradient from " ++ inputFile
            visualizeDensityGradient inputFile "ulam_density_gradient.png"
        _ -> putStrLn "Usage: program 1 <max_num> | program 2 <nth> | program 3 <input_file> | program 4 <input_file> | program 5 <input_file>"
    endTime <- getCurrentTime
    putStrLn $ "Execution time: " ++ show (diffUTCTime endTime startTime)

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
                 | y' == k  -> base + 2 * k + (k - x')
                 | x' == -k -> base + 4 * k + (k - y')
                 | y' == -k -> base + 6 * k + (k + x')
                 | otherwise -> 0

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

analyzeAndHighlightImage :: FilePath -> FilePath -> IO ()
analyzeAndHighlightImage inputFile outputFile = do
    putStrLn $ "Loading image from " ++ inputFile
    imgResult <- readImage inputFile
    case imgResult of
        Left err -> putStrLn $ "Error loading image: " ++ err
        Right dynImg -> do
            let img = convertImage dynImg
            let highlightedImg = highlightDiagonalsAndClusters img
            savePngImage outputFile (ImageRGB8 highlightedImg)
            putStrLn $ "Highlighted image saved as " ++ outputFile

visualizeDiagonalsGradient :: FilePath -> FilePath -> IO ()
visualizeDiagonalsGradient inputFile outputFile = do
    putStrLn $ "Loading image from " ++ inputFile
    imgResult <- readImage inputFile
    case imgResult of
        Left err -> putStrLn $ "Error loading image: " ++ err
        Right dynImg -> do
            let img = convertImage dynImg
            let gradientImg = applyDiagonalGradient img
            savePngImage outputFile (ImageRGB8 gradientImg)
            putStrLn $ "Diagonals gradient image saved as " ++ outputFile

visualizeDensityGradient :: FilePath -> FilePath -> IO ()
visualizeDensityGradient inputFile outputFile = do
    putStrLn $ "Loading image from " ++ inputFile
    imgResult <- readImage inputFile
    case imgResult of
        Left err -> putStrLn $ "Error loading image: " ++ err
        Right dynImg -> do
            let img = convertImage dynImg
            let gradientImg = applyDensityGradient img
            savePngImage outputFile (ImageRGB8 gradientImg)
            putStrLn $ "Density gradient image saved as " ++ outputFile

convertImage :: DynamicImage -> Image PixelRGB8
convertImage dynImg = case dynImg of
    ImageRGBA8 img -> pixelMap rgbaToRGB8 img
    ImageRGB8 img -> img
    ImageY8 img -> pixelMap (\p -> PixelRGB8 p p p) img
    ImageY16 img -> pixelMap (\p -> let p' = fromIntegral (p `div` 256) in PixelRGB8 p' p' p') img
    ImageYF img -> pixelMap (\p -> let p' = floor (p * 255) in PixelRGB8 p' p' p') img
    ImageYA8 img -> pixelMap (\(PixelYA8 y _) -> PixelRGB8 y y y) img
    ImageYA16 img -> pixelMap (\(PixelYA16 y _) -> let y' = fromIntegral (y `div` 256) in PixelRGB8 y' y' y') img
    _ -> error "Unsupported image format"

rgbaToRGB8 :: PixelRGBA8 -> PixelRGB8
rgbaToRGB8 (PixelRGBA8 r g b _) = PixelRGB8 r g b

highlightDiagonalsAndClusters :: Image PixelRGB8 -> Image PixelRGB8
highlightDiagonalsAndClusters img = generateImage highlightPixel (imageWidth img) (imageHeight img)
  where
    primes = findPrimeCoordinates img
    diagPrimes = findDiagonalsWithMostPrimes primes
    clusterPrimes = findClustersWithHighPrimeDensity primes
    highlightPixel :: Int -> Int -> PixelRGB8
    highlightPixel x y
      | (x, y) `elem` diagPrimes = PixelRGB8 255 0 0  -- Highlight diagonals in red
      | (x, y) `elem` clusterPrimes = PixelRGB8 0 255 0  -- Highlight clusters in green
      | otherwise = pixelAt img x y

findPrimeCoordinates :: Image PixelRGB8 -> [(Int, Int)]
findPrimeCoordinates img = [(x, y) | x <- [0..imageWidth img - 1], y <- [0..imageHeight img - 1], isPrimePixel (pixelAt img x y)]
  where
    isPrimePixel (PixelRGB8 r g b) = r == 0 && g == 0 && b == 0  -- Assuming prime pixels are black

findDiagonalsWithMostPrimes :: [(Int, Int)] -> [(Int, Int)]
findDiagonalsWithMostPrimes primes = concat $ take 1 $ reverse $ sortBy (comparing length) $ groupBy ((==) `on` diagGroup) $ sortBy (comparing diagGroup) primes
  where
    diagGroup (x, y) = x - y

findClustersWithHighPrimeDensity :: [(Int, Int)] -> [(Int, Int)]
findClustersWithHighPrimeDensity primes = concat $ take 1 $ reverse $ sortBy (comparing length) $ groupBy ((==) `on` clusterGroup) $ sortBy (comparing clusterGroup) primes
  where
    clusterSize = 10  -- Define the size of clusters
    clusterGroup (x, y) = (x `div` clusterSize, y `div` clusterSize)

applyDiagonalGradient :: Image PixelRGB8 -> Image PixelRGB8
applyDiagonalGradient img = generateImage gradientPixel (imageWidth img) (imageHeight img)
  where
    primes = findPrimeCoordinates img
    diagCounts1 = countDiagonals primes (\(x, y) -> x - y)
    diagCounts2 = countDiagonals primes (\(x, y) -> x + y)
    maxCount = maximum (map snd (diagCounts1 ++ diagCounts2))
    gradientPixel :: Int -> Int -> PixelRGB8
    gradientPixel x y =
        let diagIndex1 = x - y
            diagIndex2 = x + y
            count1 = lookupDiagCount diagIndex1 diagCounts1
            count2 = lookupDiagCount diagIndex2 diagCounts2
            intensity = scaleToIntensity (max count1 count2) maxCount
        in PixelRGB8 intensity intensity intensity

countDiagonals :: [(Int, Int)] -> ((Int, Int) -> Int) -> [(Int, Int)]
countDiagonals primes f = map (\g -> (fst (head g), length g)) 
                           (groupBy ((==) `on` fst) 
                           (sortBy (comparing fst) 
                           (map (\p -> (f p, p)) primes)))

lookupDiagCount :: Int -> [(Int, Int)] -> Int
lookupDiagCount i lst = maybe 0 id . lookup i $ lst

scaleToIntensity :: Int -> Int -> Pixel8
scaleToIntensity count maxCount = fromIntegral (count * 255 `div` maxCount)

applyDensityGradient :: Image PixelRGB8 -> Image PixelRGB8
applyDensityGradient img = generateImage gradientPixel (imageWidth img) (imageHeight img)
  where
    primes = findPrimeCoordinates img
    densityMap = calculateDensity primes
    maxDensity = maximum (map snd densityMap)
    gradientPixel :: Int -> Int -> PixelRGB8
    gradientPixel x y =
        let density = lookupDensity (x `div` 10, y `div` 10) densityMap
            intensity = scaleToIntensity density maxDensity
        in PixelRGB8 intensity intensity intensity

calculateDensity :: [(Int, Int)] -> [((Int, Int), Int)]
calculateDensity = map (\g -> (fst (head g), length g)) 
                  . groupBy ((==) `on` fst) 
                  . sortBy (comparing fst) 
                  . map (\(x, y) -> ((x `div` 10, y `div` 10), (x, y)))

lookupDensity :: (Int, Int) -> [((Int, Int), Int)] -> Int
lookupDensity coord lst = maybe 0 id . lookup coord $ lst
