import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (sort)

-- | If all values are Just, extract the values
allOrNothing :: Eq a => [Maybe a] -> Maybe [a]
allOrNothing list
	| Nothing `elem` list = Nothing
	| otherwise = Just $ catMaybes list

-- | Get the first integer list (every odd element)
firstList :: [Integer] -> [Integer]
firstList (x:_:xs) = x:firstList xs
firstList _ = []

-- | Get the second integer list (every even element)
secondList :: [Integer] -> [Integer]
secondList (_:y:ys) = y:secondList ys
secondList _ = []

-- | Convert the number list into an a list of pairs
toLists :: [Integer] -> [(Integer, Integer)]
toLists list = zip ((sort . firstList) list) ((sort . secondList) list)

-- | Calculate the sum for part 1
calcSum :: [(Integer, Integer)] -> Integer
calcSum = foldr ((+) . abs . uncurry (-)) 0

-- | Parse the two input lists from raw file data
parseLists :: String -> Maybe [(Integer, Integer)]
parseLists = fmap toLists . allOrNothing . fmap readMaybe . words

-- | Calculate part 1 result
part1 :: String -> Maybe Integer
part1 = fmap calcSum . parseLists

-- | Output result
output :: Maybe Integer -> IO ()
output Nothing = print "Invalid input"
output (Just result) = print result

main :: IO ()
main = readFile "input/day1.txt" >>= output . part1
