import Data.List (foldl', sort)
import Data.Map (fromList, (!))

main = do
  let filePath = "day_8.txt"
  fileContents <- readFile filePath
  let input_lines = lines fileContents

  let result_1 = solve_1 input_lines
  putStrLn $ "Problem #1 solution is " ++ show result_1

  let result_2 = solve_2 input_lines
  putStrLn $ "Problem #2 solution is " ++ show result_2

solve_1 lines = steps instructions' nodesMap' 0 "AAA"
  where
    instructions' = instructions $ head lines
    nodesMap' = nodesMap nodes
    nodes = map toNode $ drop 2 lines

solve_2 lines = lcm' stepCounts
  where
    instructions' = instructions $ head lines
    nodesMap' = nodesMap nodes
    nodes = map toNode $ drop 2 lines
    startingNodes = findStartingNodes $ map from nodes
    stepCounts = map (steps instructions' nodesMap' 0) startingNodes

steps _ _ stepCount [_, _, 'Z'] = stepCount
steps instructions' nodesMap stepCount nodeId = steps (tail instructions') nodesMap (stepCount + 1) newNode
  where
    newNode = if instruction == 'L' then goLeft currentNode else goRight currentNode
    currentNode = nodesMap ! nodeId
    instruction = head instructions'

findStartingNodes = filter isStarting
  where
    isStarting node = last node == 'A'

lcm' = foldl' lcm 1

instructions instr = concat $ repeat instr

nodesMap nodes = fromList $ map toPair nodes
  where
    toPair node = (from node, node)

toNode :: [Char] -> Node
toNode line = Node from' goLeft' goRight'
  where
    splitted = words line
    from' = head splitted
    goLeft' = init . tail $ splitted !! 2
    goRight' = init $ last splitted

data Node = Node {from :: [Char], goLeft :: [Char], goRight :: [Char]} deriving (Show)
