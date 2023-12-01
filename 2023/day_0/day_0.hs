import Data.Char (isSpace)
import Data.List (sort)
import Data.Maybe ()

main = do
  let filePath = "day_0.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 lines = 0

solve_2 lines = 0
