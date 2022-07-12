module Lexer where

data LexError
    = UnclosedStringError Token
    | UnclosedCharError Token
    | UnrecognizedEscapeError Token
    | UnhandledStateError Token
    deriving Show

data Handled a
    = Ok a
    | Error LexError
    deriving Show

data Location = Location
    { index    :: Integer
    , line     :: Integer
    , column   :: Integer
    , fileName :: String
    , fileText :: String
    }

instance Show Location where
    show (Location _ line column fileName _) = fileName ++ ":" ++ show line ++ ":" ++ show column

startlocation :: String -> String -> Location
startlocation name text = Location 0 0 0 name text

next :: Location -> Location
next (Location index line column fileName fileText)
    | fromIntegral index >= length fileText    = Location (index + 1) line       (column + 1) fileName fileText
    | fileText !! fromIntegral (index) == '\n' = Location (index + 1) (line + 1) 0            fileName fileText
    | otherwise                                = Location (index + 1) line       (column + 1) fileName fileText

data Token
    = SymbolToken     Location String
    | IdentifierToken Location String
    | OperatorToken   Location String
    | StringToken     Location String
    | CharToken       Location Char  
    | NumberToken     Location String
    | NewLineToken    Location
    | EndOfFile       Location
    deriving Show

data LexerState
    = NoState
    | StringState
    | CharState
    | WordState
    | OpState
    | NumState

whitespace = " \v\t\f"
letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
numbers = "0123456789"
symbols = "~`!@#$%^&*()_-+={}[]|\\:;<,>.?/"

word_symbols = "\'_"
reserved_chars = "()[]{}\"\',"

-- Like cons (:) operator for lists, but for Handled lists and preserves its error state
infixr ->:
(->:) :: a -> Handled [a] -> Handled [a]
(->:) x (Ok xs) = Ok (x:xs)
(->:) x (Error err) = Error err

tokenize :: String -> String -> Handled [Token]
tokenize name code = lexer NoState (startlocation name code) (code ++ " ") [] where
    -- TODO: store both start and end positions in tokens rather than just end
    -- TODO: store tokens in errors
    lexer :: LexerState -> Location -> String -> String -> Handled [Token]
    lexer NoState l ('\"':cs) _ = lexer StringState (next l) cs []
    lexer NoState l ('\'':cs) _ = lexer CharState (next l) cs []
    lexer NoState l ('\n':cs) _ = NewLineToken l ->: lexer NoState (next l) cs []
    lexer NoState l (c:cs) _ | c `elem` reserved_chars = SymbolToken l [c] ->: lexer NoState (next l) cs []
                             | c `elem` letters        = lexer WordState l (c:cs) []
                             | c `elem` symbols        = lexer OpState   l (c:cs) []
                             | c `elem` numbers        = lexer NumState  l (c:cs) []
                             | c `elem` whitespace     = lexer NoState   (next l) cs []

    lexer WordState l (c:cs) s | c `elem` (letters ++ numbers ++ word_symbols) = lexer WordState (next l) cs (s ++ [c])
                               | otherwise = IdentifierToken l s ->: lexer NoState l (c:cs) []

    lexer OpState l (c:cs) s | c `elem` symbols && c `notElem` reserved_chars = lexer OpState (next l) cs (s ++ [c])
                             | otherwise = OperatorToken l s ->: lexer NoState l (c:cs) []
    
    lexer NumState l (c:cs) s | ('.' `notElem` s && c `elem` ('.':numbers)) || ('.' `elem` s && c `elem` numbers) = lexer NumState (next l) cs (s ++ [c])
                              | otherwise = NumberToken l s ->: lexer NoState l (c:cs) []
    
    lexer StringState l [] _ = Error UnclosedStringError
    lexer StringState l ('\\':cs) s = escapeSeq StringState (next l) cs s
    lexer StringState l (c:cs) s | c /= '\"' = lexer StringState (next l) cs (s ++ [c])
                                 | otherwise = StringToken l s ->: lexer NoState (next l) cs []

    lexer CharState l [] _ = Error UnclosedCharError
    lexer CharState l (c:cs) s | length s > 1 = Error UnclosedCharError
    lexer CharState l ('\\':cs) s = escapeSeq CharState (next l) cs s
    lexer CharState l (c:cs) s | c /= '\'' = lexer CharState (next l) cs (s ++ [c])
                               | otherwise = CharToken l (head s) ->: lexer NoState (next l) cs []

    lexer _ l [] _ = Ok [EndOfFile l]
    lexer _ _ _ _  = Error UnhandledStateError

    escapeSeq :: LexerState -> Location -> String -> String -> Handled [Token]
    escapeSeq state l ('a':cs)  s = lexer state (next l) cs (s ++ "\a")
    escapeSeq state l ('b':cs)  s = lexer state (next l) cs (s ++ "\b")
    escapeSeq state l ('f':cs)  s = lexer state (next l) cs (s ++ "\f")
    escapeSeq state l ('n':cs)  s = lexer state (next l) cs (s ++ "\n")
    escapeSeq state l ('r':cs)  s = lexer state (next l) cs (s ++ "\r")
    escapeSeq state l ('s':cs)  s = lexer state (next l) cs (s ++ " ")
    escapeSeq state l ('t':cs)  s = lexer state (next l) cs (s ++ "\t")
    escapeSeq state l ('v':cs)  s = lexer state (next l) cs (s ++ "\v")
    escapeSeq state l ('\\':cs) s = lexer state (next l) cs (s ++ "\\")
    escapeSeq _ _ _ _ = Error UnrecognizedEscapeError
