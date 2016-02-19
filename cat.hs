
indexChars :: [Char] -> [(Char, Int, Int)]
indexChars c = rindexChars c 0 0

rindexChars :: [Char] -> Int ->  Int -> [(Char, Int, Int)]
rindexChars [] _ _ = []
rindexChars (c:cs) r col
    | c == '\n' = (c, r, col) : (rindexChars cs (r + 1) 0)
    | otherwise = (c, r, col) : (rindexChars cs r (col + 1))

main = fmap (show . indexChars) fstring >>= putStrLn
        where fstring = readFile "test"
