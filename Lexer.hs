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
    = SymbolToken Symbol
    | IdentifierToken String
    | OperatorToken String
    | StringToken String
    | CharToken Char
    | NumberToken String
    | EndOfFile
    deriving Show
    
data Symbol
    = NewLineSymbol
    | IndentSymbol
    | BackslashSymbol
    | DotAccessSymbol
    | CommaSymbol
    | SemicolonSymbol
    | VertBarSymbol
    | MatchSymbol
    | ColonSymbol
    | ArrowSymbol
    | ArrowBarSymbol
    | ArrowConstraintSymbol
    | LeftParenSymbol
    | RightParenSymbol
    | LeftBracketSymbol
    | RightBracketSymbol
    | LeftCurlyBraceSymbol
    | RightCurlyBraceSymbol
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
symbols = "~`!@#$%^&*()_-+={[}]|\\:;<,>.?/"

(->:) :: a -> Handled [a] -> Handled [a]
(->:) x (Ok xs) = Ok (x:xs)
(->:) x (Error err) = Error err

tokenize :: String -> Handled [Token]
tokenize code = lexer NoState code [] where
    lexer :: LexerState -> String -> String -> Handled[Token]
    lexer NoState ('\"':cs) _ = lexer StringState cs []
    lexer NoState ('\'':cs) _ = lexer CharState cs []
    lexer NoState ('\n':cs) _ = SymbolToken NewLineSymbol ->: lexer NoState cs []
    lexer NoState (c:cs) _
        | c `elem` letters = lexer WordState (c:cs) []
        | c `elem` symbols = lexer OpState (c:cs) []
        | c `elem` numbers = lexer NumState (c:cs) []
        | c `elem` whitespace = lexer NoState cs []

    lexer WordState (c:[]) s = lexer WordState (c:" ") s
    lexer WordState (c:cs) s
        | c `elem` (letters ++ numbers) = lexer WordState cs (s ++ [c])
        | otherwise = IdentifierToken s ->: lexer NoState (c:cs) []

    lexer OpState (c:[]) s = lexer OpState (c:" ") s
    lexer OpState (c:cs) s
        | c `elem` symbols = lexer OpState cs (s ++ [c])
        | otherwise = OperatorToken s ->: lexer NoState (c:cs) []
    
    lexer NumState (c:[]) s = lexer NumState (c:" ") s
    lexer NumState (c:cs) s
        | ('.' `notElem` s && c `elem` ('.':numbers)) || ('.' `elem` s && c `elem` numbers) = lexer NumState cs (s ++ [c])
        | otherwise = NumberToken s ->: lexer NoState (c:cs) []
    
    lexer _ [] _ = Ok [EndOfFile]
    lexer _ _ _ = Error UnhandledStateError

-- Implement escape sequences
-- Implement string interpolation
lexStr :: String -> Handled String
lexStr [] = Error UnclosedStringError
lexStr ('"':cs) = Ok []
lexStr (c:cs) = insertChar c (lexStr cs)

insertChar :: Char -> Handled String -> Handled String
insertChar c (Ok str) = Ok (c:str)
insertChar c (Error err) = Error err