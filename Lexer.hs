module Lexer where

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
