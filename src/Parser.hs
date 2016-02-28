module Parser (tokenize, indexChars, Indexed (..)) where 

import Control.Applicative
import Data.Monoid

newtype Indexed a = Indexed (a, Int, Int) deriving Show

instance Monoid m => Monoid (Indexed m) where
    mempty = Indexed (mempty, 0, 0)
    mappend (Indexed (a, r, c)) (Indexed (b, _, _)) = Indexed (mappend a b, r, c)

instance Functor Indexed where
    fmap f (Indexed (a,r,c)) = Indexed (f a, r, c)

--Split && index chars
indexChars :: String -> [Indexed Char]
indexChars c = rindexChars c 1 1

rindexChars :: String -> Int ->  Int -> [Indexed Char]
rindexChars [] _ _ = []
rindexChars (c:cs) r col
    | c == '\n' = Indexed (c, r, col) : rindexChars cs (r + 1) 1
    | otherwise = Indexed (c, r, col) : rindexChars cs r (col + 1)


--split indexed chars into list of indexed strings
exDelims = " \t\n"
inDelims = "()"

chunkWith :: ([a] -> ([b],[a])) -> [a] -> [[b]]
chunkWith _ [] = []
chunkWith f xs = let (h,t) = f xs
                    in h : chunkWith f t

symChunker :: [Indexed Char] -> ([Indexed String],[Indexed Char])
symChunker [] = ([],[])
symChunker a@(s@(Indexed (ch,r,c)):ss)
    | ch `elem` inDelims = ([Indexed (ch:"",r,c)],ss)
    | otherwise = rsymChunker a

rsymChunker :: [Indexed Char] -> ([Indexed String],[Indexed Char])
rsymChunker [] = ([],[])
rsymChunker (ic@(Indexed (ch, r, c)):ics)
    | ch `elem` exDelims = ([],ics)
    | ch `elem` inDelims = ([],ic:ics)
    | otherwise = let (syms, rest) = rsymChunker ics
                    in (Indexed (ch:"",r,c):syms, rest)

tokenize :: String -> [Indexed String]
tokenize str = map mconcat chnkdSyms
    where chnkdSyms = chunkWith symChunker $ indexChars str
