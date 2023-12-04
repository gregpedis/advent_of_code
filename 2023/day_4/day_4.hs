import Data.Char (isDigit)
import Data.List (intersect)
import Data.Map (Map, fromList, update, (!))
import qualified Data.Map (map)
import Data.Text (pack, strip, unpack)

main = do
  let filePath = "day_4.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 lines = sum $ map (cardPoints . parseLine) lines

solve_2 lines = calculateCopies $ map parseLine lines

cardPoints card
  | power == 0 = 0
  | otherwise = 2 ^ (power -1)
  where
    power = matching card

parseLine line = Card cardId winning' mine'
  where
    cardId = read $ filter isDigit left
    winning' = numbers w
    mine' = numbers m
    (left, rest) = break (== ':') line
    (w, m) = break (== '|') rest
    numbers x = map read $ filter (not . null) $ splitOn ' ' (tail x)

calculateCopies cards = sum $ Data.Map.map howManyCopies calculatedCardMap
  where
    calculatedCardMap = foldl increaseCounters cardMap [1 .. length cards]
    cardMap = fromList $ map kvPair cards
    kvPair c = (id' c, CardEntry (id' c) (matching c) 1)

increaseCounters map' cardId = foldl apply map' [1 .. howManyWins cardEntry]
  where
    cardEntry = map' ! cardId
    currId = id'' cardEntry
    increment = howManyCopies cardEntry
    apply map'' offset = update f (currId + offset) map''
    f entry = Just entry {howManyCopies = howManyCopies entry + increment}

matching card = length $ winning card `intersect` mine card

splitOn :: Char -> String -> [String]
splitOn delimiter value
  | null rest = [curr]
  | otherwise = curr : splitOn delimiter (tail rest)
  where
    (curr, rest) = break (== delimiter) value

strip' = unpack . strip . pack

data Card = Card {id' :: Int, winning :: [Int], mine :: [Int]} deriving (Show)

data CardEntry = CardEntry {id'' :: Int, howManyWins :: Int, howManyCopies :: Int} deriving (Show)