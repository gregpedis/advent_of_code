import Data.Char (isSpace)
import Data.List (sort)

main = do
  let filePath = "day_1.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 lines = maximum $ groupByElf lines

solve_2 lines = sum . take 3 . reverse . sort $ groupByElf lines

groupByElf lines = fst $ foldr collect ([], 0) lines
  where
    collect line (res, currentSum)
      | isSeparator line = (currentSum : res, 0)
      | otherwise = (res, currentSum + read line :: Int)
    isSeparator line = null line || all isSpace line