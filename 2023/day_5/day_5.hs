import Data.Char (isSpace)
import Data.List (isInfixOf, sort, tails)
import Data.Text (pack, strip, unpack)

main = do
  let filePath = "day_5.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 lines = minimum locations
  where
    locations = map solveSeed seeds'
    data' = parseInput lines
    seeds' = seeds data'
    solveSeed seed =  foldl findNext seed $ maps data'

solve_2 lines = minimum locations
  where
    locations = map solveSeed seeds'
    data' = parseInput lines
    seeds' = concat . generateRanges $ seeds data'
    solveSeed seed = foldl findNext seed $ maps data'

findNext seed maps = firstOrDefault f seed maps
  where
    f map' =
      let diff = seed - sourceFrom map'
       in if diff < length' map' && diff >= 0
            then Just $ destFrom map' + diff
            else Nothing

parseInput lines = Data seeds' maps'
  where
    seeds' = numbers $ head lines
    maps' = reverse $ uncurry (:) parsedMaps
    parsedMaps = (parseMaps . drop 2) lines

parseMaps = foldl folder ([], [])
  where
    folder (curr, res) line
      | ":" `isInfixOf` line = (curr, res)
      | null line = ([], reverse curr : res)
      | otherwise = (parseMap line : curr, res)

parseMap line = Map' (head abc) (head $ tail abc) (last abc)
  where
    abc = map read $ filter (not . null) $ splitOn ' ' line

firstOrDefault _ default' [] = default'
firstOrDefault f default' (x : xs) = case f x of
  Just v -> v
  Nothing -> firstOrDefault f default' xs

strip' = unpack . strip . pack

numbers x = map read $ filter (not . null) $ splitOn ' ' values
  where
    values = tail $ dropWhile (/= ':') x

generateRanges (x:y:rest)   = [x..x+y-1] : generateRanges rest
generateRanges _           = []

splitOn delimiter value
  | null rest = [curr]
  | otherwise = curr : splitOn delimiter (tail rest)
  where
    (curr, rest) = break (== delimiter) value

data Data = Data {seeds :: [Integer], maps :: [[Map']]} deriving (Show)

data Map' = Map' {destFrom :: Integer, sourceFrom :: Integer, length' :: Integer} deriving (Show)