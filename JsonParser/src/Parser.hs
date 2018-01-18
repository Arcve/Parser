module Parser where

import Basic
import Data.List
import Data.Char
import Data.Functor((<$>))
import Data.Maybe (fromMaybe)
import Control.Monad as M
import Control.Applicative
import JsonValue
import System.IO
import System.Environment

newtype Parser a = P { run :: String -> Either String (a , String) } 


instance Functor Parser where
    f `fmap` p = P (\s -> case run p s of
        Left s' -> Left s'
        Right (a, s') -> Right (f a, s'))

instance Applicative Parser where 
    pure a  = P (\s -> Right (a, s))
    f <*> p = P (\s -> case run f s of
        Left s' -> Left s'
        Right (f', s') -> run (f' <$> p) s')

instance Alternative Parser where
    empty = P (\s ->Left "Alternative")
    p <|> q = P (\s -> case run p s of
        Left _ -> run q s
        Right (a, s') -> Right (a, s'))

instance Monad Parser where
    return a = P (\s -> Right (a, s))
    p >>= f  = P (\s -> case run p s of
            Left s' -> Left s'
            Right (a, s') -> run (f a) s') 


char :: Parser Char
char = P (\s -> case s of
    []-> Left "empty string"
    (x:xs) -> Right (x, xs))

list :: Parser a -> Parser [a]
list p = list1 p <|> pure []

list1 :: Parser a -> Parser [a]
list1 p = p >>= (\x -> list p >>= (\ xs -> return (x : xs))) 

unexpectedCharParser :: Char -> Parser Char
unexpectedCharParser c = P (\c -> Left ("unexpectedChar " ++ show c))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = 
    do c <- char
       if p c then return c else unexpectedCharParser c

is :: Char -> Parser Char
is = satisfy . (==)

digit ::Parser Char
digit = satisfy isDigit

space ::Parser Char
space = satisfy isSpace

spaces ::Parser String
spaces =list space

spaces1 ::Parser String
spaces1 =list1 space

upper ::Parser Char
upper =satisfy isUpper

alpha ::Parser Char
alpha =satisfy isAlpha


thisMany ::Int-> Parser a-> Parser [a]
thisMany = M.replicateM


tok ::
 Parser a
 -> Parser a
tok p = 
 do v <- p
    spaces
    pure v

charTok ::
    Char
    -> Parser Char
charTok = tok . is

commaTok ::Parser Char
commaTok =charTok ','

quote ::Parser Char
quote = is '\'' <|> is '"'

string ::String-> Parser String
string = traverse is

stringTok ::String-> Parser String
stringTok s =
    do v <- string s
       spaces
       pure v

oneof :: String-> Parser Char
oneof s = satisfy (`elem` s)

between ::Parser o-> Parser c-> Parser a-> Parser a
between p1 p2 p3=
    do p1
       v <- p3 
       p2
       pure v

betweenCharTok ::Char-> Char-> Parser a-> Parser a
betweenCharTok a b = between (charTok a) (charTok b)



hex ::Parser Char
hex = let hInt a = fromMaybe 0 (readHex a)
      in chr . hInt <$> M.replicateM 4 (satisfy isHexDigit)

hexu ::Parser Char
hexu = do is 'u'
          hex  
          
sepby1 ::Parser a-> Parser s-> Parser [a]
sepby1 a s = 
    do x <- a
       xs <- list ( s >> a )
       pure (x:xs)

sepby ::Parser a-> Parser s-> Parser [a]
sepby a s = sepby1 a s <|> pure []

betweenSepbyComma ::Char-> Char-> Parser a-> Parser [a]
betweenSepbyComma a b p=
    betweenCharTok a b $ sepby p $ charTok ','

data SpecialCharacter =
    BackSpace
    | FormFeed
    | NewLine
    | CarriageReturn
    | Tab
    | VerticalTab
    | SingleQuote
    | DoubleQuote
    | Backslash
    deriving (Eq, Ord, Show)
  
fromSpecialCharacter ::
    SpecialCharacter
    -> Char
fromSpecialCharacter BackSpace =
    chr 0x08
fromSpecialCharacter FormFeed =
    chr 0x0C
fromSpecialCharacter NewLine =
    '\n'
fromSpecialCharacter CarriageReturn =
    '\r'
fromSpecialCharacter Tab =
    '\t'
fromSpecialCharacter VerticalTab =
    '\v'
fromSpecialCharacter SingleQuote =
    '\''
fromSpecialCharacter DoubleQuote =
    '"'
fromSpecialCharacter Backslash =
    '\\'

toSpecialCharacter ::Char-> Maybe SpecialCharacter
toSpecialCharacter c =
    let table = [('b', BackSpace),
                ('f', FormFeed),
                ('n', NewLine),
                ('r', CarriageReturn) ,
                ('t', Tab) ,
                ('v', VerticalTab) ,
                ('\'', SingleQuote) ,
                ('"' , DoubleQuote) ,
                ('\\', Backslash)]                
    in snd <$> find ((==) c . fst) table

jsonString ::Parser String
jsonString =
    let str =
          do c1 <- char
             if c1 == '\\' 
                then 
                 do c2 <- char
                    if c2 == 'u' 
                      then 
                          hex
                    else  
                      case toSpecialCharacter c2 of 
                            Just a -> return (fromSpecialCharacter a)
                            Nothing  -> unexpectedCharParser c2 
              else 
                if c1 == '"'
                  then 
                    unexpectedCharParser '"'
                  else 
                    return c1
        in between (is '"') (charTok '"') (list str)

jsonNumber ::Parser Rational
jsonNumber =
    P (\i -> 
        case readFloats i of 
            Nothing ->Left ("UnexpectedString" ++ i)
            Just (a, r) -> Right (a, r))

jsonTrue ::Parser String
jsonTrue =stringTok "true"

jsonFalse::Parser String
jsonFalse =stringTok "false"

jsonNull::Parser String
jsonNull =stringTok "null"

jsonArray ::Parser [JsonValue]
jsonArray = betweenSepbyComma '[' ']' jsonValue

jsonObject ::Parser Assoc
jsonObject =
    betweenSepbyComma '{' '}' (
    do k <- jsonString
       charTok ':'
       v <- jsonValue
       return (k,v)
    )

jsonValue ::Parser JsonValue
jsonValue =
    spaces *>
    (JsonNull <$ jsonNull
    <|> JsonTrue <$ jsonTrue
    <|> JsonFalse <$ jsonFalse
    <|> JsonString <$> jsonString      
    <|> JsonArray <$> jsonArray
    <|> JsonObject <$> jsonObject
    <|> JsonRational <$> jsonNumber)

readJsonValue ::FilePath-> IO (Either String (JsonValue, String))
readJsonValue = (<$>) (run jsonValue)  . readFile

