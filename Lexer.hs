module Lexer where

data LexError
    = UnclosedStringError
    | UnhandledStateError
    deriving Show

data Handled a
    = Ok a
    | Error LexError
    deriving Show

data Token
    = SymbolToken String
    | IdentifierToken String
    | OperatorToken String
    | StringToken String
    | CharToken Char
    | NumberToken String
    | NewLineToken
    | EndOfFile
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

tokenize :: String -> Handled [Token]
tokenize code = lexer NoState (code ++ " ") [] where
    lexer :: LexerState -> String -> String -> Handled [Token]
    lexer NoState ('\"':cs) _ = lexer StringState cs []
    lexer NoState ('\'':cs) _ = lexer CharState cs []
    lexer NoState ('\n':cs) _ = NewLineToken ->: lexer NoState cs []
    lexer NoState (c:cs) _ | c `elem` reserved_chars = SymbolToken [c] ->: lexer NoState cs []
                           | c `elem` letters        = lexer WordState (c:cs) []
                           | c `elem` symbols        = lexer OpState (c:cs) []
                           | c `elem` numbers        = lexer NumState (c:cs) []
                           | c `elem` whitespace     = lexer NoState cs []

    lexer WordState (c:cs) s | c `elem` (letters ++ numbers ++ word_symbols) = lexer WordState cs (s ++ [c])
                             | otherwise = IdentifierToken s ->: lexer NoState (c:cs) []

    lexer OpState (c:cs) s | c `elem` symbols && c `notElem` reserved_chars = lexer OpState cs (s ++ [c])
                           | otherwise = OperatorToken s ->: lexer NoState (c:cs) []
    
    lexer NumState (c:cs) s | ('.' `notElem` s && c `elem` ('.':numbers)) || ('.' `elem` s && c `elem` numbers) = lexer NumState cs (s ++ [c])
                            | otherwise = NumberToken s ->: lexer NoState (c:cs) []
    
    -- Implement escape sequences
    -- Implement string interpolation
    lexer StringState [] _ = Error UnclosedStringError
    lexer StringState ('\\':cs) s = escapeSeq StringState cs s
    lexer StringState (c:cs) s | c /= '\"' = lexer StringState cs (s ++ [c])
                               | otherwise = StringToken s ->: lexer NoState cs []
    
    lexer _ [] _ = Ok [EndOfFile]
    lexer _ _ _  = Error UnhandledStateError

    escapeSeq :: LexerState -> String -> String -> Handled [Token]
    escapeSeq state ('a':cs) s = lexer state cs (s ++ "\a")
    escapeSeq state ('b':cs) s = lexer state cs (s ++ "\b")
    escapeSeq state ('f':cs) s = lexer state cs (s ++ "\f")
    escapeSeq state ('n':cs) s = lexer state cs (s ++ "\n")
    escapeSeq state ('r':cs) s = lexer state cs (s ++ "\r")
    escapeSeq state ('s':cs) s = lexer state cs (s ++ " ")
    escapeSeq state ('t':cs) s = lexer state cs (s ++ "\t")
    escapeSeq state ('v':cs) s = lexer state cs (s ++ "\v")

insertChar :: Char -> Handled String -> Handled String
insertChar c (Ok str) = Ok (c:str)
insertChar c (Error err) = Error err