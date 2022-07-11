module Parser where

import qualified Lexer

data Type
    = Type String
    | Generic [Type]

data Expression
    = Literal Literal
    | Function [Expression]
    | BinOp Expression Expression

data Literal
    = IdentifierLit String
    | IntegerLit String
    | FloatLit String
    | StringLit String
    | CharLit Char
