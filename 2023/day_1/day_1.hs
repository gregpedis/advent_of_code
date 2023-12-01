import Data.Char (isDigit)
import Data.List (find, isSuffixOf, take)
import Data.Map (fromList, keys, lookup)
import Data.Maybe (fromMaybe)
import Data.Set (fromList)
import Prelude hiding (lookup)

main = do
  let filePath = "day_1.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 = sum . map solveLine

solve_2 = sum . map (solveLine . fixLine)
  where
    fixLine = foldl fix ""
    fix acc curr =
      let newAcc = acc ++ [curr]
          suffix = find (`isSuffixOf` newAcc) keys'
       in case suffix of
            Just key -> replace key newAcc
            Nothing -> newAcc
    replace key current =
      let value = fromMaybe 42 $ lookup key kvPairs :: Int -- this should always be Just number
       in -- need to put the key back for overlaps, e.g. twone -> 2twone -> 2tw1one
          take (length current - length key) current ++ show value ++ key
    kvPairs =
      Data.Map.fromList
        [ ("one", 1),
          ("two", 2),
          ("three", 3),
          ("four", 4),
          ("five", 5),
          ("six", 6),
          ("seven", 7),
          ("eight", 8),
          ("nine", 9)
        ]
    keys' = Data.Set.fromList . keys $ kvPairs

solveLine line =
  let digits = filter isDigit line
   in read (head digits : [last digits]) :: Int
