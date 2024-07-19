import Data.List (sort)
import Data.Map as Map (fromList, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (pack, strip, unpack)

main = do
  let filePath = "day_2.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 = solve roundResult

solve_2 = solve roundResult2

solve roundRes lines = sum $ map (roundRes . parsePair) lines

parsePair line = (head pairList, last pairList)
  where
    pairList = words . unpack . strip . pack $ line

roundResult (themCode, usCode) = partialResult + us
  where
    partialResult
      | them == us = 3
      | them == 1 && us == 3 = 0
      | them == 3 && us == 1 = 6
      | us > them = 6
      | otherwise = 0
    them = lookup' themCode
    us = lookup' usCode
    lookup' key = fromMaybe (-1) $ Map.lookup key rpsValues
    rpsValues =
      Map.fromList
        [ ("A", 1),
          ("B", 2),
          ("C", 3),
          ("X", 1),
          ("Y", 2),
          ("Z", 3)
        ]

roundResult2 (themCode, resultCode) = partialResult + us
  where
    us
      | partialResult == 3 = them
      | partialResult == 0 = if them == 1 then 3 else them -1
      | otherwise = if them == 3 then 1 else them + 1
    them = lookup' themCode
    partialResult = lookup' resultCode
    lookup' key = fromMaybe (-1) $ Map.lookup key rpsValues
    rpsValues =
      Map.fromList
        [ ("A", 1),
          ("B", 2),
          ("C", 3),
          ("X", 0),
          ("Y", 3),
          ("Z", 6)
        ]