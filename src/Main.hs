import Parser

main = fmap (show . filt . tokenize . indexChars) fstring >>= putStrLn
        where fstring = readFile "../test"
              filt = filter (\(Indexed (s, _)) -> s /= [])
