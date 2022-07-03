from enum import Enum, auto
from typing import List

class Type(Enum):
    SYM = auto() # Symbols (Identifiers, operators)
    BRC = auto() # Brackets () [] {}
    NUM = auto() # Numbers (23, 1.2)
    CHR = auto() # Characters ('a')
    STR = auto() # Strings ("Hello, world!")
    EOF = auto() # End-of-file

class Token:
    def __init__(self, type: Type, name: any = None) -> None:
        self.type = type
        self.name = name

def lex(code: str) -> List[Token]:
    return
