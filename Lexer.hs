module Lexer where

data LexError
    = UnclosedStringError
    | UnclosedCharError
    | UnrecognizedEscapeError
    | UnhandledStateError
    deriving Show

data Handled a
    = Ok a
    | Error LexError Location
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

startLocation :: String -> String -> Location
startLocation = Location 0 0 0

next :: Location -> Location
next (Location index line column fileName fileText)
    | fromIntegral index >= length fileText  = Location (index + 1) line       (column + 1) fileName fileText
    | fileText !! fromIntegral index == '\n' = Location (index + 1) (line + 1) 0            fileName fileText
    | otherwise                              = Location (index + 1) line       (column + 1) fileName fileText

data Token
    = SymbolToken     Location Location String
    | IdentifierToken Location Location String
    | OperatorToken   Location Location String
    | StringToken     Location Location String
    | CharToken       Location Location Char  
    | NumberToken     Location Location String
    | NewLineToken    Location
    | EndOfFile       Location
    deriving Show

data LexerState
    = NoState
    | StringState Location
    | CharState   Location
    | WordState   Location
    | OpState     Location
    | NumState    Location

whitespace = " \v\t\f"
letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
numbers = "0123456789"
symbols = "~`!@#$%^&*()_-+={}[]|\\:;<,>.?/"

wordSymbols = "\'_"
reservedChars = "()[]{}\"\',"

-- Like cons (:) operator for lists, but for Handled lists and preserves its error state
infixr ->:
(->:) :: a -> Handled [a] -> Handled [a]
(->:) x (Ok xs) = Ok (x:xs)
(->:) x (Error err l) = Error err l

tokenize :: String -> String -> Handled [Token]
tokenize name code = lexer NoState (startLocation name code) (code ++ " ") [] where
    -- TODO: fix precise positioning of tokens
    lexer :: LexerState -> Location -> String -> String -> Handled [Token]
    lexer NoState l ('\"':cs) _ = lexer (StringState l) (next l) cs []
    lexer NoState l ('\'':cs) _ = lexer (CharState l) (next l) cs []
    lexer NoState l ('\n':cs) _ = NewLineToken l ->: lexer NoState (next l) cs []
    lexer NoState l (c:cs) _ | c `elem` reservedChars = SymbolToken l l [c] ->: lexer NoState (next l) cs []
                             | c `elem` letters       = lexer (WordState l) l (c:cs) []
                             | c `elem` symbols       = lexer (OpState   l) l (c:cs) []
                             | c `elem` numbers       = lexer (NumState  l) l (c:cs) []
                             | c `elem` whitespace    = lexer NoState (next l) cs []

    lexer (WordState sl) l (c:cs) s | c `elem` (letters ++ numbers ++ wordSymbols) = lexer (WordState sl) (next l) cs (s ++ [c])
                                    | otherwise = IdentifierToken sl l s ->: lexer NoState l (c:cs) []

    lexer (OpState sl) l (c:cs) s | c `elem` symbols && c `notElem` reservedChars = lexer (OpState sl) (next l) cs (s ++ [c])
                                  | otherwise = OperatorToken sl l s ->: lexer NoState l (c:cs) []
    
    lexer (NumState sl) l (c:cs) s | ('.' `notElem` s && c `elem` ('.':numbers)) || ('.' `elem` s && c `elem` numbers) = lexer (NumState sl) (next l) cs (s ++ [c])
                                   | otherwise = NumberToken sl l s ->: lexer NoState l (c:cs) []
    
    lexer (StringState sl) l [] _ = Error UnclosedStringError l
    lexer (StringState sl) l ('\\':cs) s = escapeSeq (StringState sl) (next l) cs s
    lexer (StringState sl) l (c:cs) s | c /= '\"' = lexer (StringState sl) (next l) cs (s ++ [c])
                                      | otherwise = StringToken sl l s ->: lexer NoState (next l) cs []

    lexer (CharState sl) l [] _ = Error UnclosedCharError l
    lexer (CharState sl) l (c:cs) s | length s > 1 = Error UnclosedCharError l
    lexer (CharState sl) l ('\\':cs) s = escapeSeq (CharState sl) (next l) cs s
    lexer (CharState sl) l (c:cs) s | c /= '\'' = lexer (CharState sl) (next l) cs (s ++ [c])
                                    | otherwise = CharToken sl l (head s) ->: lexer NoState (next l) cs []

    lexer _ l [] _ = Ok [EndOfFile l]
    lexer _ l _  _ = Error UnhandledStateError l

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
    escapeSeq _     l _         _ = Error UnrecognizedEscapeError l
