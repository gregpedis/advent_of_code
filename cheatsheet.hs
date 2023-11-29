-- Resources

-- http://learn.hfm.io/
-- http://learnyouahaskell.com/chapters
-- https://www.haskell.org/tutorial/
-- http://book.realworldhaskell.org/read/

-- Types

-- Haskell is strongly, statically typed.

-- Type annotation on simple values, optional:
-- 42::Int, 'h'::Char, True::Bool

-- Type signature on simple values, optional:
-- someInt::Int, someChar::Char, someBool::Bool

-- Here are some primitives:

-- Numeric
intValue :: Int -- type signature
intValue = 42 :: Int -- variable definition

integerValue = 5 :: Integer -- same as Int, but arbitrary precision

floatValue = 42.69 :: Float

doubleValue = 3.14159 :: Double

-- Character
charValue = 'h'

-- [Char] and String are interchangable
stringValue = "hello" :: String

stringValue' = "hello" :: [Char]

-- Boolean

boolValue = True

-- Lists

-- Lists have to have elements of the same type.
someNumbers = [1, 2, 3, 4] -- a list of integers

someDoubles = [1, 2.0] -- 1 is coerced to 1.0, so this is a list of doubles

oneToFive = [1 .. 5] -- this is the same as [1,2,3,4,5]

allEvensLessThan10 = [2, 4 .. 10] -- this is the same as [2,4,6,8,10]

allEvens = [2, 4 ..] -- this is an infinite list that starts with 2 and the step is 2.

oneTwoThree = [1, 2, 3]

oneTwoThree' = 1 : 2 : 3 : [] -- this is the same as oneTwoThree. the ':' op prepends an element

oneToTen = [1, 2, 3, 4, 5] ++ [6, 7, 8, 9, 10] -- list concatenation

_ = [1, 2, 3] !! 1 -- indexing, returns '2'. Almost never used.

_ = head [1, 2, 3] -- returns 1

_ = tail [1, 2, 3] -- returns [2,3]

_ = last [1, 2, 3] -- returns 3

_ = init [1, 2, 3] -- returns [1,2]

-- Some other useful list functions:
-- length, null, reverse, elem, cycle
-- sum, product, minimum, maximum, take, drop

-- List comprehensions
-- This is a useful syntax to create arbitrary lists with specific conditions.
-- The syntax looks like this:
-- [(x, | x <- someList, someCondition]
-- [(x1,x2..) | x1 <- someList1, x2 <- someList2.., someCondition1, someCondition2,..]

rightTriangles =
  [ (a, b, c)
    | c <- [1 .. 10],
      b <- [1 .. c],
      a <- [1 .. b],
      a ^ 2 + b ^ 2 == c ^ 2,
      a + b + c == 24
  ]

-- Tuples

pair = (1, 2)

_ = fst pair -- returns 1

_ = snd pair -- returns 2

pairs = zip [1, 2] [3, 4] -- returns [(1,3), (2,4)]

-- Typeclasses

-- Haskell groups sets of types in what is called typeclasses.
-- You can kind of think of them as interfaces.
-- For example, float, double, int, and integer all belong to the Num typeclass.
-- That is useful because the Num typeclass contains functions for numeric ops.
-- For example, the following method can be used to any type that belongs to Num:
numericOperations :: Num a => a -> a -> a -> a -> a -> a
numericOperations a b c d e = a + b - c * d + abs e

-- As declared in the type signature,
-- this function takes 5 values belonging to a type 'a' that belongs to Num
-- and returns a value also belonging to 'a'.
-- Num is the typeclass, 'a' is called a type placeholder.
-- Because of typeclasses, 'numericOperations' is polymorphic.
-- That means it can be called with arguments of any type belonging to Num.
-- Here is a demonstration:

_ = numericOperations 1 2 3 4 5 -- Everything is coerced to Integer

_ = numericOperations 1 2 3 4 5 :: Int -- Everything is coerced to Int

_ = numericOperations 1 2 3 4 5 :: Float -- Everything is coerced to Float

_ = numericOperations 1.0 2 3 4 5 -- Everything after 1.0 is coerced to Double

-- Some important typeclasses, as well as what types belonging to them implement:

-- Show: can be converted to string, which means they can be "printed"
-- Read: opposite of show, if read they can be converted to target type
-- Eq: equality, '==' and '!='
-- Ord: ordering, '>', '>=', '<', '<='
-- Enum: sequentiality, "succ 5" is 6, "pred 42" is 41
-- Bounded: have upper+lower bound, e.g. minBound::Char, maxBound::Int
-- Num, Integral, Fractional, Floating, Complex: numerical operations

-- Functions

-- Haskell functions tells us what something is.
-- They map something(s) to some other thing(s).

-- They have to start with lowercase letter or _
-- Can only contain alphanumeric, _ and ' characters

-- Here are some examples:

-- Maps a number to the next number.
inc x = x + 1 -- function declaration

-- Same method but with type signature.
-- Optional, but provides clarity.
inc' :: Num a => a -> a -- type signature
inc' x = x + 1 -- function declaration

-- This hides the Data.List.length function.
-- Be careful to not name functions with predefined names.
-- Might cause ambiguous execution on ghci/build.
length x = x

-- Multiple arguments
myAdd :: Num a => a -> a -> a
myAdd x y = x + y

-- As seen, type signatures do not separate arguments and return value.
-- Everything is separated by a "->", because haskell curries everything.
-- This is a fancy way of saying all functions take a single argument
-- and return either a value of a function that takes a single argument, etc.
-- This also allows partial application in every single function, out of the box.
-- Here is an example:

myAdd2 = myAdd 2

-- [myAdd 2] partially applies 2 to myAdd and returns a function.
-- When [myAdd2 y] is called, [myAdd 2 y] is called, which results in 2+y
-- This can also be written in a less idiomatic but more understandable way:
myAdd2' x = myAdd 2 x -- same as myAdd2

-- Infix application
_ = 1 + 2

_ = 1 `myAdd` 2

-- Infx is left-associative, meaning these two are the same:
-- x `fun1` y `fun2` z
-- (x `fun1` y) `fun2` z

-- Prefix application
_ = (+) 1 2

_ = myAdd 1 2

-- Pattern matching
-- TODO

-- Guards
-- TODO

-- Where
-- TODO

-- Let bindings
-- Expressions that _let_ you bind values at any point in a function.
-- They are very local, meaning their scoped is very limited, e.g. only on the current guard.
multiply x y = let result = x * y in result

-- Since they are expressions, they can be used anywhere.
_ = 4 * (let a = 9 in a + 1) + 2

-- Can define more than one binding, on the same level of identation.
multiplyThenAdd x y z =
  let p = x * y
      result = p + z
   in result

-- Can be nested.
multiplyThenMultiply x y z =
  let p1 = x * y
   in let p2 = p1 * z
       in p2

-- Case
-- TODO
