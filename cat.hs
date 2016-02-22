import Control.Applicative

data Index = Index { row::Int, col::Int }
    deriving Show

instance Eq Index where
    Index {row = a, col = b} == Index {row = c, col = d} = a == c && b == d

instance Ord Index where
    Index {row = a, col = b} `compare` Index {row = c, col = d}
        | a > c = GT
        | a < c = LT
        | b > d = GT
        | b < d = LT
        | otherwise = EQ

newtype Indexed a = Indexed (a, Index)
    deriving Show

instance Functor Indexed where
    fmap f (Indexed (a, i)) = Indexed (f a, i)

type Symbol = Indexed Char
type Token = Indexed String

addSymb :: Symbol -> Token -> Token
addSymb (Indexed (c, _)) t = fmap (strv++) t
    where strv = pure c

newInd a r c = Indexed (a, Index {row = r, col = c})

indexChars :: [Char] -> [Symbol]
indexChars c = rindexChars c 0 0

rindexChars :: [Char] -> Int ->  Int -> [Symbol]
rindexChars [] _ _ = []
rindexChars (c:cs) r col
    | c == '\n' = newInd c r col : (rindexChars cs (r + 1) 0)
    | otherwise = newInd c r col : (rindexChars cs r (col + 1))

delims = "\n\t ()"

main = fmap (show . indexChars) fstring >>= putStrLn
        where fstring = readFile "test"
