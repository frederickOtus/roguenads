import Control.Applicative

newtype Indexed a = Indexed {getSym :: (a, Int, Int) }
    deriving Show

instance Functor Indexed where
    fmap f (Indexed (x, r, c)) = Indexed ((f x), r, c)

instance Applicative Indexed where
    pure x = Indexed (x, 0, 0)
    (Indexed (x, r, c)) <*> ind = fmap x ind

type Symbol = Indexed Char
type Token = Indexed String

a = Indexed ('a', 0, 0)
b = Indexed ('b', 0, 1)
l = Indexed ([], 0, 0) :: Indexed [Char]

aa = Indexed ("a", 0, 0)
bb = Indexed ("b", 0, 0)

--indexChars :: [Char] -> [Symbol]
--indexChars c = rindexChars c 0 0

--rindexChars :: [Char] -> Int ->  Int -> [Symbol]
--rindexChars [] _ _ = []
--rindexChars (c:cs) r col
--    | c == '\n' = (c, r, col) : (rindexChars cs (r + 1) 0)
--    | otherwise = (c, r, col) : (rindexChars cs r (col + 1))

--delims = "\n\t ()"

--main = fmap (show . indexChars) fstring >>= putStrLn
--        where fstring = readFile "test"
