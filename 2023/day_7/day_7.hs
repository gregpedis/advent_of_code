import Data.Char (digitToInt)
import Data.List (group, sort)

main = do
  let filePath = "day_7.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 lines = solve lines toBet
  where
    toBet line = parseBet line 70 groupCards

solve_2 lines = solve lines toBet
  where
    toBet line = parseBet line (-70) groupCards2

solve lines toBet = sum $ map winning zipped
  where
    winning (rank, bet) = rank * bid bet
    zipped = zip [1 ..] bets
    bets = sort $ map toBet lines

parseBet line jokerValue groupBy = Bet {cards = cardsToInts cards' jokerValue, bid = read $ last splitted, label = getLabel cards' groupBy}
  where
    splitted = words line
    cards' = head splitted

data Bet = Bet {cards :: [Int], bid :: Int, label :: Label} deriving (Eq, Show)

getLabel cards' groupBy = case groupBy cards' of
  [5] -> Five
  [1, 4] -> Four
  [2, 3] -> FullHouse
  [1, 1, 3] -> Three
  [1, 2, 2] -> Two
  [1, 1, 1, 2] -> Pair
  _ -> High

groupCards cards' = sort $ map length $ group $ sort cards'

groupCards2 cards' = applyJokerLogic cards' (groupCards cards')

applyJokerLogic cards' grouped =
  if 'J' `elem` cards' && length grouped > 1
    then appied
    else grouped
  where
    toRemove = head grouped
    remaining = tail grouped
    appied = case grouped of
      [1, 2, 2] -> if onlyOneJoker then [2, 3] else [1, 4] -- i should be fired
      _ -> init remaining ++ [last remaining + toRemove]
    onlyOneJoker = length (filter (=='J') cards') == 1

cardsToInts cards' jokerValue = map toInt cards'
  where
    toInt c = case c of
      'A' -> 100
      'K' -> 90
      'Q' -> 80
      'J' -> jokerValue
      'T' -> 60
      _ -> digitToInt c

instance Ord Bet where
  compare x y =
    let rankCompare = label x `compare` label y
     in if rankCompare == EQ
          then cards x `compare` cards y
          else rankCompare

data Label = High | Pair | Two | Three | FullHouse | Four | Five deriving (Eq, Show, Ord)
