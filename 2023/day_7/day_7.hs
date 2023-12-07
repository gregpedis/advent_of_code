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

solve_1 lines = sum $ map winning zipped
  where
    winning (rank, bet) = rank * bid bet
    zipped = zip [1 ..] bets
    bets = sort $ map toBet lines

solve_2 lines = 0

toBet line = Bet {cards = head splitted, bid = read $ last splitted}
  where
    splitted = words line

data Bet = Bet {cards :: [Char], bid :: Int} deriving (Eq, Show)

getLabel bet = case grouped of
  [5] -> Five
  [1, 4] -> Four
  [2, 3] -> FullHouse
  [1, 1, 3] -> Three
  [1, 2, 2] -> Two
  [1, 1, 1, 2] -> Pair
  _ -> High
  where
    grouped = sort $ map length $ group . sort $ cards bet

cardsToInts bet = map toInt (cards bet)
  where
    toInt c = case c of
      'A' -> 100
      'K' -> 90
      'Q' -> 80
      'J' -> 70
      'T' -> 60
      _ -> digitToInt c

instance Ord Bet where
  compare x y =
    let rankCompare = getLabel x `compare` getLabel y
     in if rankCompare == EQ
          then cardsToInts x `compare` cardsToInts y
          else rankCompare
    where
      rx = getLabel x
      ry = getLabel y

data Label = Five | Four | FullHouse | Three | Two | Pair | High deriving (Eq, Show)

instance Ord Label where
  compare x y = toInt x `compare` toInt y

toInt label = case label of
  Five -> 5
  Four -> 4
  FullHouse -> 3.5
  Three -> 3
  Two -> 2
  Pair -> 1
  High -> 0