import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.Text (pack, strip, unpack)

main = do
  let filePath = "day_2.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 lines = sum . map id' $ filter isValid $ map parseLine lines

solve_2 lines = sum $ map (power . parseLine) lines

parseLine line = Game {id' = gameId, red = maximum reds, green = maximum greens, blue = maximum blues}
  where
    parts = let x = splitOn ':' line in (strip' $ head x, splitOn ';' $ strip' $ last x)
    gameId = extractNumber $ fst parts
    rounds = map parseRound $ snd parts
    (reds, greens, blues) = unzip3 rounds -- transpose

parseRound r = (red, green, blue)
  where
    parts = splitOn ',' r
    getAmount color =
      let found = filter (color `isInfixOf`) parts
       in if null found then 0 else extractNumber $ head found
    red = getAmount "red"
    green = getAmount "green"
    blue = getAmount "blue"

power game = red game * green game * blue game

isValid game = red game <= 12 && green game <= 13 && blue game <= 14

extractNumber x = read $ filter isDigit x :: Int

strip' = unpack . strip . pack

splitOn delimiter line = let res = foldl split ([], "") line in fst res ++ [snd res]
  where
    split (acc, intermediate) curr
      | curr == delimiter = (acc ++ [intermediate ++ [curr]], "")
      | otherwise = (acc, intermediate ++ [curr])

data Game = Game {id' :: Int, red :: Int, green :: Int, blue :: Int} deriving (Show)