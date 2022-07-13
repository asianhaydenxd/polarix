module Parser where

import Lexer

type ModuleLabel = String

data Module = Module ModuleLabel [ModuleLabel] [Declaration]

data Declaration
    = Function
    | Struct
    | Enum

data Type
    = Type String
    | Generic [Type]

data Expression
    = Literal Literal
    | FunctionOp [Expression]
    | BinOp Expression Expression

data Literal
    = IdentifierLit String
    | IntegerLit String
    | FloatLit String
    | StringLit String
    | CharLit Char

parse :: [Token] -> Handled Module
parse ts = undefined
