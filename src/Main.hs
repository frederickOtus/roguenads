import Parser

main = fmap (show . lexify) fstring >>= putStrLn
        where fstring = readFile "../test"
