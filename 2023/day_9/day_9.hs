import Data.Char (isSpace)
import Data.List (sort, tails)

main = do
  let filePath = "day_9.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 lines = sum $ map solve lines
  where
    solve = nextValue . diffs . parseLine

solve_2 lines = sum $ map solve lines
  where
    solve = nextValue . diffs . reverse . parseLine

parseLine line = map read $ words line

diffs sequence = diffs' [sequence]
  where
    diffs' sequences = if finished then reverse sequences else diffs' (diff : sequences)
      where
        finished = all (== 0) curr
        diff = [b - a | (a : b : _) <- tails curr]
        curr = head sequences

nextValue diffLists = foldr f 0 $ init diffLists
  where
    f values diff = last values + diff