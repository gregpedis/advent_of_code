import Data.Char (ord)
import Data.Map (Map, elems, fromList, insert, lookup, (!))

main = do
  let filePath = "day_15.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 lines = sum res
  where
    res = map applyHash steps
    steps = splitOn ',' $ head lines

solve_2 lines = calculateFocusingPower state
  where
    state = applyInstructions instructions
    instructions = map parseInstruction steps
    steps = splitOn ',' $ head lines

applyHash = foldl hash' 0
  where
    hash' acc curr = (acc + ord curr) * 17 `mod` 256

parseInstruction chars
  | last chars == '-' = Remove (init chars)
  | otherwise = parseUpdate chars
  where
    parseUpdate chars =
      let splitted = splitOn '=' chars
       in Update (head splitted) (read $ last splitted)

applyInstructions = foldl f initialBoxes
  where
    f state instr =
      let newBox = applyInstruction state instr
       in insert (boxId newBox) newBox state
    initialBoxes = fromList [(id', Box id' []) | id' <- [0 .. 255]]

applyInstruction state instruction = case instruction of
  Remove lens' -> removeLens state $ lens instruction
  Update lens' focal' -> updateLens state instruction

removeLens state lens' = Box boxId' newContents
  where
    newContents = removeFromList $ contents box
    box = state ! boxId'
    boxId' = applyHash lens'
    removeFromList [] = []
    removeFromList (z : zs) = if lens' == lens z then zs else z : removeFromList zs

updateLens state instr = Box boxId' newContents
  where
    newContents = updateInPlace $ contents box
    box = state ! boxId'
    boxId' = applyHash $ lens instr
    updateInPlace [] = [instr]
    updateInPlace (z : zs) = if lens instr == lens z then instr : zs else z : updateInPlace zs

calculateFocusingPower state = sum $ concatMap boxStrength boxes
  where
    boxes = elems state
    boxStrength box = mapWithIndex lensStrength $ contents box
      where
        lensStrength idx instr = (1 + boxId box) * idx * focal instr

mapWithIndex f xs = map f' zs
  where
    f' = uncurry f
    zs = zip [1 ..] xs

splitOn delimiter value
  | null rest = [curr]
  | otherwise = curr : splitOn delimiter (tail rest)
  where
    (curr, rest) = break (== delimiter) value

data Instruction
  = Remove {lens :: String}
  | Update {lens :: String, focal :: Int}
  deriving (Show)

data Box = Box {boxId :: Int, contents :: [Instruction]} deriving (Show)