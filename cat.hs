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

ind = Index {row=0, col=0}
exDelims = " \t\n"
inDelims = "()"

has :: Eq a => [a] -> a -> Bool
has l elm = foldr (||) False $ map (==elm) l

baseToken :: Symbol -> Token
baseToken (Indexed (c,i)) = Indexed ([c],i)

chunkWith :: ([a] -> ([a],[a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h,t) = f xs
                    in h : chunkWith f t

symChunker :: [Symbol] -> ([Symbol],[Symbol])
symChunker [] = ([],[])
symChunker a@(s@(Indexed (c, i)):ss)
    | has inDelims c = ([s],ss)
    | otherwise = rsymChunker a

rsymChunker :: [Symbol] -> ([Symbol],[Symbol])
rsymChunker [] = ([],[])
rsymChunker (s@(Indexed (c, i)):ss)
    | has exDelims c = ([],ss)
    | has inDelims c = ([],s:ss)
    | otherwise = let (syms, rest) = rsymChunker ss
                    in (s:syms, rest)

chunkSyms = chunkWith symChunker
tokenize :: [Symbol] -> [Token]
tokenize syms = map toTokens chnkdSyms
    where chnkdSyms = chunkSyms syms
          toTokens = foldr addSymb (newInd "" 0 0 :: Token)

newInd a r c = Indexed (a, Index {row = r, col = c})

indexChars :: [Char] -> [Symbol]
indexChars c = rindexChars c 0 0

rindexChars :: [Char] -> Int ->  Int -> [Symbol]
rindexChars [] _ _ = []
rindexChars (c:cs) r col
    | c == '\n' = newInd c r col : (rindexChars cs (r + 1) 0)
    | otherwise = newInd c r col : (rindexChars cs r (col + 1))

main = fmap (show . filt . tokenize . indexChars) fstring >>= putStrLn
        where fstring = readFile "test"
              filt = filter (\(Indexed (s, _)) -> s /= [])
