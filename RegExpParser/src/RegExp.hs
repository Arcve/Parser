module RegExp where

import Control.Applicative
data RegExp =
     Lit Char
    | Any
    | Star  RegExp
    | QMark RegExp
    | Or RegExp RegExp
    | Str [RegExp]
    deriving (Show, Eq)

newtype Parser a = P { run :: String ->  Maybe (a , String) } 

instance Functor Parser where
    f `fmap` p = P (\s -> case run p s of
        Nothing  -> Nothing
        Just (a, s') -> Just (f a, s'))

instance Applicative Parser where 
    pure a  = P (\s -> Just (a, s))
    f <*> p = P (\s -> case run f s of
        Nothing -> Nothing
        Just (f', s') -> run (f' <$> p) s')

instance Alternative Parser where
    empty = P  (const Nothing)
    p <|> q = P (\s -> case run p s of
        Nothing -> run q s
        Just (a, s') -> Just (a, s'))

instance Monad Parser where
    return a = P (\s -> Just (a, s))
    p >>= f  = P (\s -> case run p s of
            Nothing -> Nothing
            Just (a, s') -> run (f a) s') 

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P f 
        where f [] = Nothing
              f (x:xs) = if p x then Just (x,xs) else Nothing

is :: Char -> Parser Char
is = satisfy . (==)

noneOf :: String -> Parser Char
noneOf s = satisfy (`notElem` s)

lit, anything, str, atom ,qmark, star, factor, term, rexp :: Parser RegExp


lit = Lit <$> noneOf "().*?|"
anything = Any <$ is '.'
str = is '(' *> rexp <* is ')'

atom = lit <|> anything <|> str

qmark = QMark <$> (atom  <* is '?')
star  = Star  <$> (atom  <* is '*')

factor = qmark <|> star <|> atom
term = (do
        x <- factor
        xs <- some factor
        return $ Str (x:xs)) <|> factor

rexp = (do 
        p <- term 
        is '|'
        q <- term
        return $ Or p q) <|> term

parseExp :: String -> Maybe RegExp
parseExp s = case run rexp s of
                Just (x,[]) -> Just x
                _ -> Nothing

interpreter :: RegExp -> String -> (String -> Bool) -> Bool
interpreter exp s k = case (exp, s) of
        (Lit a, []) -> False
        (Lit a, x:xs) -> a == x && k xs
        (Any , []) -> False
        (Any , _:xs) -> k xs
        (Str [e], _) -> interpreter e s k
        (Str (e:es), _) -> interpreter e s (\xs -> interpreter (Str es) xs k)
        (Star e, _) -> k s || interpreter e s (\xs -> interpreter exp xs k)
        (QMark e, _) -> k s || interpreter e s k
        (Or e1 e2, _) -> interpreter e1 s k || interpreter e2 s k
       
match :: String -> String -> Bool
match p s = case parseExp p of
                 Nothing -> False
                 Just x -> interpreter x s null