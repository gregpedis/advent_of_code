import Data.Char (isUpper, ord)
import Data.Set (findMin, fromList, intersection)

main = do
  let filePath = "day_3.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

-- parentheses are needed, or map is only applied on itemValue.
solve_1 = sum . map (itemValue . appearsInBoth)

solve_2 lines = sum . map (itemValue . appearsInAll) $ splitEvery 3 lines

appearsInBoth rucksack = findMin $ intersection first second
  where
    first = fromList $ take halfSize rucksack
    second = fromList $ drop halfSize rucksack
    halfSize = length rucksack `div` 2

-- translates ord 'a' from 97 to 1
-- translates ord 'A' from 65 to 27
itemValue c = offset . ord $ c
  where
    offset x = if isUpper c then x - 38 else x - 96

appearsInAll rucksacks = findMin $ foldl1 intersection sets
  where
    sets = map fromList rucksacks

splitEvery n [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)