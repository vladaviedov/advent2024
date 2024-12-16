import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

-- | If all values are Just, extract the values
allOrNothing :: Eq a => [Maybe a] -> Maybe [a]
allOrNothing list
	| Nothing `elem` list = Nothing
	| otherwise = Just $ catMaybes list

-- | Output result
output :: Maybe Integer -> IO ()
output Nothing = print "Invalid input"
output (Just result) = print result

-- | Parse the input from the file
parseFile :: String -> Maybe [[Integer]]
parseFile = allOrNothing . map (allOrNothing . map readMaybe . words) . lines

-- | Check if list is in ascending order
ascCheck :: [(Integer, Integer)] -> Bool
ascCheck = all (uncurry (<=))

-- | Check if list is in a descending order
descCheck :: [(Integer, Integer)] -> Bool
descCheck = all (uncurry (>=))

-- | Check that difference is between 1 and 3
diffCheck :: [(Integer, Integer)] -> Bool
diffCheck = all ((\x -> x >= 1 && x <= 3) . abs . uncurry (-))

-- | Safety filter
safetyFilter :: [Integer] -> Bool
safetyFilter xs = (\x -> diffCheck x && (ascCheck x || descCheck x)) $ zip xs $ tail xs

-- Part 1 computation
part1 :: String -> Maybe Integer
part1 = fmap (toInteger . length . filter safetyFilter) . parseFile

-- Part 2 computation
part2 :: String -> Maybe Integer
part2 _ = Just 0

main :: IO ()
main = do
	input <- readFile "input/day2.txt"
	(output . part1) input
	(output . part2) input
