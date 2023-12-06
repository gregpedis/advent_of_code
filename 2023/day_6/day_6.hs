import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (pack, strip, unpack)

main = do
  let filePath = "day_6.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 lines = product $ map solveLine zipped
  where
    zipped = zip times distances
    times = numbers $ head lines
    distances = numbers $ last lines

solve_2 lines = solveLine (t, d)
  where
    t = number $ head lines
    d = number $ last lines
    number line = read $ filter isDigit line :: Int

solveLine (t, d) = atMost - atLeast + 1
  where
    atLeast = fromMaybe 0 $ find' t d [1 .. t -1]
    atMost = fromMaybe 0 $ find' t d $ reverse [1 .. t -1]
    find' t d = let isWin th = th * (t - th) > d in find isWin

strip' = unpack . strip . pack

numbers x = map read $ filter (not . null) $ splitOn ' ' values
  where
    values = tail $ dropWhile (/= ':') x

splitOn delimiter value
  | null rest = [curr]
  | otherwise = curr : splitOn delimiter (tail rest)
  where
    (curr, rest) = break (== delimiter) value