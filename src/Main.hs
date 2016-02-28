import Parser

main = fmap (show . filt . tokenize) fstring >>= putStrLn
        where fstring = readFile "../test"
              filt = filter (\(Indexed (s, _, _)) -> s /= [])
