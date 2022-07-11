module Parser where

import qualified Lexer

data Literal
    = IdentifierLit String
    | IntegerLit String
    | FloatLit String
    | StringLit String
    | CharLit Char

data Expression
    = Literal Literal
    | Function [Expression]
    | BinOp Expression Expression
