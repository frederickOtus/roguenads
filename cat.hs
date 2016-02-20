type Symbol = (Char, Int, Int)
type Token = (String, Int, Int)

indexChars :: [Char] -> [Symbol]
indexChars c = rindexChars c 0 0

rindexChars :: [Char] -> Int ->  Int -> [Symbol]
rindexChars [] _ _ = []
rindexChars (c:cs) r col
    | c == '\n' = (c, r, col) : (rindexChars cs (r + 1) 0)
    | otherwise = (c, r, col) : (rindexChars cs r (col + 1))

delims = "\n\t ()"

main = fmap (show . indexChars) fstring >>= putStrLn
        where fstring = readFile "test"
