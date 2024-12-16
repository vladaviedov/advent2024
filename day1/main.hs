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
toPairLists :: [Integer] -> [(Integer, Integer)]
toPairLists list = zip ((sort . firstList) list) ((sort . secondList) list)

-- | Calculate the sum for part 1
calcSum :: [(Integer, Integer)] -> Integer
calcSum = foldr ((+) . abs . uncurry (-)) 0

-- | Parse the combined input list from raw file data
parseFullList :: String -> Maybe [Integer]
parseFullList = allOrNothing . map readMaybe . words

-- | Calculate part 1 result
part1 :: String -> Maybe Integer
part1 = fmap (calcSum . toPairLists) . parseFullList

-- | Convert the number list into two lists
toLists :: [Integer] -> ([Integer], [Integer])
toLists list = (,) (firstList list) (secondList list)

-- | Count occurances of a single element
countEl :: Integer -> [Integer] -> Integer
countEl e = toInteger . length . filter (==e)

-- | Calculate similarity for a single element
similarityEl :: Integer -> [Integer] -> Integer
similarityEl e = (*e) . countEl e

-- | Calculate the similarity score
calcSimilarity :: [Integer] -> [Integer] -> Integer
calcSimilarity x y = foldr ((+) . (`similarityEl` y)) 0 x

part2 :: String -> Maybe Integer
part2 = fmap (uncurry calcSimilarity . toLists) . parseFullList

-- | Output result
output :: Maybe Integer -> IO ()
output Nothing = print "Invalid input"
output (Just result) = print result

main :: IO ()
main = do
	input <- readFile "input/day1.txt"
	(output . part1) input
	(output . part2) input
