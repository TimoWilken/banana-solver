module Main where

import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import System.IO
import System.Environment
import System.Random
import Text.Printf

{- COUNTER -}

type Counter a = Map.Map a Int

increment :: Ord a => a -> Counter a -> Counter a
increment = Map.alter $ Just . maybe 1 (+1)

decrement :: Ord a => a -> Counter a -> Counter a
decrement = Map.alter $ eliminateIfZero . fmap (subtract 1)
  where eliminateIfZero Nothing = Nothing
        eliminateIfZero (Just n) = if n == 0 then Nothing else Just n

fromWord :: String -> Counter Char
fromWord "" = mempty
fromWord (c:cs) = increment c $ fromWord cs

isSubcounterOf :: Ord a => Counter a -> Counter a -> Bool
isSubcounterOf = Map.isSubmapOfBy (<=)

(\\\) :: Ord a => Counter a -> Counter a -> Counter a
(\\\) = Map.differenceWith (\v1 v2 -> if v1 <= v2 then Nothing else Just $ v1 - v2)

{- WORD GRID -}

data BBox = BBox Int Int Int Int deriving (Eq, Show)
data Direction = Down | Across deriving (Eq, Show)
data Position = Position Int Int deriving (Eq, Show)

offsetPos :: Int -> Direction -> Position -> Position
offsetPos offset Down (Position x y) = Position x (y + offset)
offsetPos offset Across (Position x y) = Position (x + offset) y

succPos :: Direction -> Position -> Position
succPos = offsetPos 1

predPos :: Direction -> Position -> Position
predPos = offsetPos (-1)

surrPos :: Direction -> Position -> [Position]
surrPos dir pos = [predPos dir pos, succPos dir pos]

type WordList = [String]
type WordGrid = [(Position, Direction, String)]

usedGridWords :: WordGrid -> WordList
usedGridWords [] = []
usedGridWords ((_, _, w):ws) = w : usedGridWords ws

allGridWords :: WordGrid -> WordList
allGridWords grid = filter ((>1) . length)
  $ concatMap (concatMap continuousWords) [rows, transpose rows]
  where rows = gridRows grid
        continuousWords [] = []
        continuousWords rowOrCol = map fromJust justs : continuousWords rest
          where (justs, rest) = span isJust $ dropWhile isNothing rowOrCol

insertWord :: Position -> Direction -> String -> WordGrid -> Either String WordGrid
insertWord pos dir w grid =
  if conflicts then Left $ w ++ " conflicts with " ++ showLetters existingLetters
  else Right ((pos, dir, w) : grid)
  where existingLetters = letters pos dir (length w) grid
        showLetters = map $ fromMaybe '_'
        conflicts = any letterConflicts $ zip existingLetters w
        letterConflicts (gridL, wordL) = maybe False (/=wordL) gridL

singletonGrid :: String -> WordGrid
singletonGrid word = grid
  where (Right grid) = insertWord (Position 0 0) Across word []

letterAt :: Position -> WordGrid -> Maybe Char
letterAt (Position x y) = listToMaybe . mapMaybe letterInWord
  where letterInWord entry@(_, _, w) = (w!!) <$> coordInWord entry
        coordInWord (Position wx wy, d, w)
          | inWord x wx y wy Down = Just $ y - wy
          | inWord y wy x wx Across = Just $ x - wx
          | otherwise = Nothing
          where inWord p wp q wq = (&&coordsGood) . (==d)
                  where coordsGood = p == wp && q >= wq && q - wq < length w

letters :: Integral a => Position -> Direction -> a -> WordGrid -> [Maybe Char]
letters _ _ 0 _ = mempty
letters pos dir len grid =
  letterAt pos grid : letters (succPos dir pos) dir (len-1) grid

bbox :: WordGrid -> BBox
bbox [] = BBox 0 0 0 0
bbox grid = BBox minX minY (maxX-minX) (maxY-minY)
  where minX = minimum [x | (Position x _, _, _) <- grid]
        minY = minimum [y | (Position _ y, _, _) <- grid]
        maxX = maximum [x + case d of Across -> length w; Down -> 1
                       | (Position x _, d, w) <- grid]
        maxY = maximum [y + case d of Down -> length w; Across -> 1
                       | (Position _ y, d, w) <- grid]

gridRows :: WordGrid -> [[Maybe Char]]
gridRows grid = [[letterAt (Position x' y') grid | x' <- xs] | y' <- ys]
  where xs = take w [x..]; ys = take h [y..]; (BBox x y w h) = bbox grid

showGrid :: Char -> WordGrid -> String
showGrid blankChar = unlines . map (map $ fromMaybe blankChar) . gridRows

reachableLetters :: WordGrid -> [(Position, Direction, Char)]
reachableLetters grid = tagged Down ++ tagged Across
  where tagged dir = map (\(pos, c) -> (pos, dir, c))
          $ filter (freeInDir . fst) filledPositions
          where freeInDir = any isNothing . map (`letterAt` grid) . surrPos dir
        filledPositions = [(Position x' y', fromJust mc) | x' <- xs, y' <- ys,
                           let mc = letterAt (Position x' y') grid, isJust mc]
          where xs = take w [x..]; ys = take h [y..]; (BBox x y w h) = bbox grid

allWordsValid :: WordList -> WordGrid -> Bool
allWordsValid wordList = all (`elem` wordList) . allGridWords

{- MAIN LOGIC -}

longestFormableWords :: Counter Char -> WordList -> WordList
longestFormableWords haveLetters = sortBy reverseByLen . filter formable
  where formable = flip isSubcounterOf haveLetters . fromWord
        reverseByLen = flip compare `on` length

solveGrid :: Counter Char -> WordList -> [WordGrid]
solveGrid haveLetters wordList = solveGridStage mempty haveLetters
  where solveGridStage :: WordGrid -> Counter Char -> [WordGrid]
        solveGridStage grid lett
          | null lett = [grid | allWordsValid wordList grid]
          | null grid = concatMap completeFromStart wordList
          | otherwise = concatMap extendUsingLetter $ reachableLetters grid
          where completeFromStart w =
                  solveGridStage (singletonGrid w) $ lett \\\ fromWord w
                extendUsingLetter l@(_, _, c) = concatMap (extendUsingWordAt l)
                  $ longestFormableWords (increment c lett) wordList
                extendUsingWordAt (Position x y, dir, c) w
                  | c `notElem` w = mempty
                  | otherwise = concatMap tryInsertWord possibleCoords
                  where possibleCoords = case dir of
                          Down -> [Position x (y-i) | i <- elemIndices c w]
                          Across -> [Position (x-i) y | i <- elemIndices c w]

                        tryInsertWord p
                          | null usingLetters = mempty
                          | otherwise = case insertWord p dir w grid of
                              Left _ -> mempty
                              Right newGrid -> solveGridStage newGrid lettersLeft
                          where existing = letters p dir (length w) grid
                                overlap = [wc | (wc, ec) <- zip w existing,
                                            ec == Just wc]
                                usingLetters = fromWord w \\\ fromWord overlap
                                lettersLeft = lett \\\ usingLetters

printGridAndWords :: (Int, WordGrid) -> IO ()
printGridAndWords (i, grid) = do
  hPrintf stdout "grid #%d: (%s)\n" i . intercalate ", " $ allGridWords grid
  putStrLn $ showGrid '.' grid

readWordList :: FilePath -> IO WordList
readWordList fname = do
  contents <- readFile fname
  return $ process contents
  -- intentionally leaving out uppercase words -- proper names or abbreviations
  where process = filter (all (`elem` ['a'..'z'])) . lines

setupWithChars :: [Char] -> String -> IO (Counter Char, WordList)
setupWithChars startChars wordListFname = do
  hPrintf stderr "using letters: %s\n" $ startChars
  wordList <- readWordList wordListFname
  return (fromWord $ map toLower startChars, wordList)

main :: IO ()
main = do
  args <- getArgs
  (startLetters, wordList) <- case args of
    "-h":_ -> error "usage: bananas [-h] WORDLIST [LETTERS]"
    "--help":_ -> error "usage: bananas [-h] WORDLIST [LETTERS]"
    [wordListFname] -> do
      gen <- getStdGen
      setupWithChars (take 11 $ randomRs ('a', 'z') gen) wordListFname
    [wordListFname, startChars] -> setupWithChars startChars wordListFname
    _ -> error "usage: bananas [-h] WORDLIST [LETTERS]"
  let prunedWordList = longestFormableWords startLetters wordList
  hPrintf stderr "word list size: %d\n" $ length prunedWordList
  traverse_ printGridAndWords $ zip [0..] $ solveGrid startLetters prunedWordList
