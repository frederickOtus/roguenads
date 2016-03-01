module Parser (lexify, tokenize, indexChars, Indexed (..)) where 

import Control.Applicative
import Data.Monoid

--the types we have 
newtype Indexed a = Indexed (a, Int, Int)

instance (Show a) => Show (Indexed a) where
    show (Indexed (elm, r, c)) = show elm ++ " : " ++ instr
        where instr = "(" ++ show r ++ "," ++ show c ++ ")"

instance Monoid m => Monoid (Indexed m) where
    mempty = Indexed (mempty, 0, 0)
    mappend (Indexed (a, r, c)) (Indexed (b, _, _)) = Indexed (mappend a b, r, c)

instance Functor Indexed where
    fmap f (Indexed (a,r,c)) = Indexed (f a, r, c)

type Token = Indexed String
type Lexeme = Indexed TypedToken

data TypedToken = LP | RP | Ident String | TInt Int | TBool Bool
    deriving Show

--Split && index chars
indexChars :: String -> [Indexed Char]
indexChars c = rindexChars c 1 1

rindexChars :: String -> Int ->  Int -> [Indexed Char]
rindexChars [] _ _ = []
rindexChars (c:cs) r col
    | c == '\n' = Indexed (c, r, col) : rindexChars cs (r + 1) 1
    | otherwise = Indexed (c, r, col) : rindexChars cs r (col + 1)


--split indexed chars into list of indexed tokens
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

tokenize :: String -> [Token]
tokenize str = map mconcat chnkdSyms
    where chnkdSyms = chunkWith symChunker $ indexChars str

--generate lexemes from the tokens

isInt = all isDig
    where isDig = flip elem ['0' .. '9']

identType :: String -> TypedToken
identType "#t" = TBool True
identType "#f" = TBool False
identType "(" = LP
identType ")" = RP
identType s
    | isInt s = TInt (read s)
    | otherwise = Ident s

toke2Lex :: Token -> Lexeme
toke2Lex (Indexed (s, r, c)) = Indexed (identType s, r, c)

lexify :: String -> [Lexeme]
lexify = map toke2Lex . filt . tokenize
    where filt = filter (\(Indexed (s, _, _)) -> s /= "")

--build ASTs

data SExpr = SList [SExpr] | SIdent String | SBool Bool | SInt Int
    deriving Show
--data TypedTokens = LP | RP | Ident String | TInt Int | TBool Bool
--type Lexeme = Indexed TypedToken

--nextExpr :: [Lexeme] -> (SExpr, [Lexeme])
--nextExpr (Indexed (Ident s,r,c):ls) = (SIdent s, ls)
--nextExpr (Indexed (TInt i,r,c):ls) = (SInt s, ls)
--nextExpr (Indexed (Ident s,r,c):ls) = (SIdent s, ls)
