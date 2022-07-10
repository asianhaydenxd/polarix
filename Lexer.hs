module Lexer where

data LexError
    = UnclosedStringError
    deriving Show

data Handled a = Ok a | Error LexError deriving Show

data Token
    = SymbolToken Symbol
    | IdentifierToken String
    | OperatorToken String
    | StringToken String
    | CharToken Char
    | NumberToken String
    | EndOfFile
    
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

whitespace = " \n\v\t\f"

lex :: String -> [Token]
lex code = undefined

-- Implement escape sequences
-- Implement string interpolation
lexStr :: String -> Handled String
lexStr [] = Error UnclosedStringError
lexStr ('"':cs) = Ok []
lexStr (c:cs) = insertChar c (lexStr cs)

insertChar :: Char -> Handled String -> Handled String
insertChar c (Ok str) = Ok (c:str)
insertChar c (Error err) = Error err