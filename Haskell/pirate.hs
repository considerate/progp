import System.Environment
import Data.Char
import Data.List

vowels = ['a', 'o', 'u', 'i', 'e', 'å', 'ä', 'ö', 'y']

isVowel :: Char -> Bool
isVowel x = (toLower x) `elem` vowels

isConsonant :: Char -> Bool
isConsonant x = isAlpha x && not (isVowel x)

pirateSpeech :: Char -> String
pirateSpeech x | isVowel x = [x]
			   | isConsonant x = [x, 'o', x]
			   | otherwise = [x]

join :: [String] -> String -> String
join parts delim = concat (intersperse delim parts)

rovarsprak :: String -> String
rovarsprak text = concat $ map pirateSpeech text

karpsravor :: [Char] -> [Char]
karpsravor [x] = [x]
karpsravor [x,y] = [x,y]
karpsravor (x:y:z:xs) = 
	if [x, y, z] == [x,'o',x]
	then [x] ++ karpsravor(xs) 
	else [x] ++ karpsravor( (y:z:xs) )
karpsravor [] = []

main :: IO ()
main = do
	args <- getArgs
	let text = join args " "
	let result = karpsravor text
	print result


