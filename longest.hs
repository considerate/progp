longest :: [String] -> String -> String
longest (word:rest) current = 
	if length word > length current
	then longest rest word
	else longest rest current
longest [] current = current

longestWord :: String -> String
longestWord text = longest (words text) ""

main :: IO ()
main = do
	contents <- getContents
	print $ length $ longestWord contents
