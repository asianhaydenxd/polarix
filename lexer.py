from enum import Enum, auto
from typing import List
from typing_extensions import Self
from unicodedata import category, lookup

# Steps:
#   Turn the string of code into tokens
#   Organize the tokens into a tree
#   Traverse tree to define functions and enum constructors
#   Traverse and execute tree

NUMBERS = "1234567890"
WHITESPACE = "\n\v\t\r "

RESERVED_CHARS = "()[]{},;\"\'"
RESERVED_OPS = ["=", ":", "|", "->", "|=>", "<=", "\\"]

RESERVED_IDS = ["module", "import", "if", "otherwise", "set", "data", "mut", "infixL", "infixR"]

class LexError(BaseException):
    def __init__(self, name, position):
        self.name = name
        self.position = position

class UnterminatedStringError(LexError):
    def __init__(self, position):
        super().__init__("UnterminatedStringError", position)

class UnterminatedCharError(LexError):
    def __init__(self, position):
        super().__init__("UnterminatedCharError", position)

class TerminatingNumericalDotError(LexError):
    def __init__(self, position):
        super().__init__("TerminatingNumericalDotError", position)

class MultipleNumericalDotsError(LexError):
    def __init__(self, position):
        super().__init__("MultipleNumericalDotsError", position)

class Position:
    def __init__(self, index: int, line: int, x: int, file_name: str, file_text: str) -> None:
        self.index = index
        self.line = line
        self.x = x
        self.file_name = file_name
        self.file_text = file_text

    def next(self) -> None:
        # Go to the next character in the text
        self.index += 1
        self.x += 1
        if self.index < len(self.file_text):
            if self.file_text[self.index] == '\n':
                self.x = 0
                self.line += 1

    def in_range(self) -> bool:
        return self.index < len(self.file_text)

    def copy(self) -> Self:
        return Position(self.index, self.line, self.x, self.file_name, self.file_text)

    def __str__(self) -> str:
        return f"line {self.line}, char {self.x}"

class TokenCategory(Enum):
    Symbol     = auto()
    Identifier = auto()
    String     = auto()
    Character  = auto()
    Number     = auto()
    NewLine    = auto()
    EndOfFile  = auto()

class Token:
    def __init__(self, category: TokenCategory, name: any = None, start: Position = None, end: Position = None) -> None:
        self.category = category
        self.name = name

        if start:
            self.start = start.copy()

        if end:
            self.end = end.copy()
        elif start:
            self.end = start.copy().next()
    
    def __str__(self) -> str:
        match self.category:
            case TokenCategory.Symbol: category = "sym"
            case TokenCategory.Identifier: category = "id"
            case TokenCategory.String: category = "str"
            case TokenCategory.Character: category = "chr"
            case TokenCategory.Number: category = "num"
            case TokenCategory.NewLine: category = "newline"
            case TokenCategory.EndOfFile: category = "eof"

        return f"[{category}: {self.name}]" if self.name is not None else f"[{category}]"

class Lexer():
    def __init__(self, code: str):
        self.code = code
        self.tokens = []
        self.pos = Position(0, 0, 0, "stdio", code)
        self.interpolateception = 0
        self.lex()

    def current_char(self) -> chr:
        try:
            return self.code[self.pos.index]
        except IndexError:
            return None
    
    def lex(self) -> List[Token]:
        while self.pos.index < len(self.code):
            if self.current_char() in NUMBERS:
                self.get_num()
            elif self.current_char() in "\n":
                if len(self.tokens) > 0:
                    if self.tokens[-1].category == TokenCategory.Symbol and self.tokens[-1].name == "\\":
                        self.tokens.pop()
                        self.pos.next()
                        continue
                self.tokens.append(Token(TokenCategory.NewLine, start=self.pos))
                self.pos.next()
            elif self.current_char() in WHITESPACE:
                self.pos.next()
            elif self.current_char() == "\"" or (self.current_char() == "}" and self.interpolateception > 0):
                self.get_string()
            elif self.current_char() == "\'":
                self.get_char()
            elif self.current_char() in RESERVED_CHARS:
                self.tokens.append(Token(TokenCategory.Symbol, self.current_char(), self.pos))
                self.pos.next()
            else:
                self.get_word()
        self.tokens.append(Token(TokenCategory.EndOfFile, start=self.pos))
        return self.tokens

    def get_num(self):
        start_pos = self.pos.copy()
        has_dot = False
        last_dot = False

        while self.pos.in_range() and self.current_char() in NUMBERS + ".":
            if self.current_char() == ".":
                if has_dot: raise MultipleNumericalDotsError(self.pos.copy())
                has_dot = True
                last_dot = True
            else:
                last_dot = False

            self.pos.next()
        
        if last_dot: raise TerminatingNumericalDotError(self.pos.copy())

        self.tokens.append(Token(
            TokenCategory.Number,
            self.code[start_pos.index : self.pos.index],
            start_pos,
            self.pos
        ))

    def get_string(self):
        if self.current_char() == "}" and self.interpolateception > 0:
            self.interpolateception -= 1
            self.tokens.append(Token(
                TokenCategory.Symbol,
                "interpolclose",
                self.pos,
                self.pos
            ))

        start_pos = self.pos.copy()

        self.pos.next()

        string = ""

        while self.current_char() != "\"":
            if self.current_char() == "\\":
                string += self.next_escape()
                continue

            # implement string interpolation "Hello, ${name}!"
            if self.current_char() == "$":
                self.pos.next()
                if self.current_char() != "{":
                    string += "$" + self.current_char()
                    continue
                else:
                    self.pos.next()
                    self.interpolateception += 1
                    self.tokens.append(Token(
                        TokenCategory.String,
                        string,
                        start_pos,
                        self.pos
                    ))
                    self.tokens.append(Token(
                        TokenCategory.Symbol,
                        "interpolopen",
                        self.pos,
                        self.pos
                    ))
                    return

            string += self.current_char()
            self.pos.next()
            if not self.pos.in_range() or self.current_char() == "\n":
                raise UnterminatedStringError(start_pos)

        self.pos.next()

        self.tokens.append(Token(
            TokenCategory.String, 
            string, 
            start_pos, 
            self.pos
        ))

    def get_char(self):
        start_pos = self.pos.copy()

        self.pos.next()

        char = self.current_char()
        if self.current_char() == "\\":
            char = self.next_escape("\'")
        else:
            self.pos.next()
        
        if self.current_char() != "\'":
            raise UnterminatedCharError(start_pos)
        
        self.pos.next()

        self.tokens.append(Token(
            TokenCategory.Character,
            char,
            start_pos,
            self.pos
        ))

    def next_escape(self, delimiter="\""):
        # String/char escaping
        string = self.code
        new_string = ""
        self.pos.next()
        if   self.current_char() == "a":  new_string += "\a"
        elif self.current_char() == "b":  new_string += "\b"
        elif self.current_char() == "e":  new_string += "\x1b"
        elif self.current_char() == "f":  new_string += "\f"
        elif self.current_char() == "n":  new_string += "\n"
        elif self.current_char() == "r":  new_string += "\r"
        elif self.current_char() == "s":  new_string += " "
        elif self.current_char() == "t":  new_string += "\t"
        elif self.current_char() == "v":  new_string += "\v"
        elif self.current_char() == "$":  new_string += "$"
        elif self.current_char() == "\\": new_string += "\\"
        elif self.current_char() == "\'": new_string += "\'"
        elif self.current_char() == "\"": new_string += "\""
        elif self.current_char() == "N":
            self.pos.next()
            if self.current_char() != "{":
                new_string += "\\N"
            else:
                self.pos.next()
                lookup_code = ""
                while self.current_char() != delimiter and self.current_char() != "}":
                    lookup_code += self.current_char()
                    self.pos.next()
                if self.current_char() == "}":
                    new_string += lookup(lookup_code)
                else:
                    new_string += "\\N" + lookup_code
        elif self.current_char() == "x":
            self.pos.next()
            if self.current_char() not in "0123456789ABCDEFabcdef":
                new_string += "\\x" + self.current_char()
            else:
                following_pos = self.pos.copy()
                following_pos.next()
                if self.code[following_pos.index] not in "0123456789ABCDEFabcdef":
                    new_string += "\\x" + self.current_char()
                else:
                    new_string += bytearray.fromhex(self.current_char() + self.code[following_pos.index]).decode()
                    self.pos.next()
        elif self.current_char() == "u":
            self.pos.next()
            digits = ""
            last_pos = None
            while self.current_char() != delimiter and len(digits) < 4 and self.current_char() in "0123456789ABCDEFabcdef":
                digits += self.current_char()
                last_pos = self.pos.copy()
                self.pos.next()
            if len(digits) == 4:
                new_string += chr(int(digits, 16))
            else:
                new_string += "\\u" + digits
            self.pos = last_pos
        elif self.current_char() == "U":
            self.pos.next()
            digits = ""
            last_pos = None
            while self.current_char() != delimiter and len(digits) < 8 and self.current_char() in "0123456789ABCDEFabcdef":
                digits += self.current_char()
                last_pos = self.pos.copy()
                self.pos.next()
            if len(digits) == 8:
                new_string += chr(int(digits, 16))
            else:
                new_string += "\\U" + digits
            self.pos = last_pos
        elif self.current_char() in "01234567":
            digits = ""
            last_pos = None
            while self.current_char() != delimiter and len(digits) < 3 and self.current_char() in "01234567":
                digits += self.current_char()
                last_pos = self.pos.copy()
                self.pos.next()
            new_string += chr(int(digits, 8))
            self.pos = last_pos
        else:
            new_string += "\\"
            new_string += self.current_char()
        self.pos.next()

        return new_string

    def get_word(self):
        unicode_category = category(self.current_char())
        start_pos = self.pos.copy()
        
        while self.pos.in_range() and self.current_char() not in RESERVED_CHARS and category(self.current_char()).replace("N","L").replace("S", "P")[0] == unicode_category.replace("S", "P")[0]:
            if self.current_char() == ".":
                last_pos = self.pos.copy()
                self.pos.next()
                if category(self.current_char()).startswith("L"):
                    self.tokens.append(Token(
                        TokenCategory.Symbol, 
                        "dotaccess", 
                        start_pos, 
                        self.pos
                    ))
                    return
                if category(self.current_char()) == "Nd":
                    self.pos = last_pos
                    return self.get_num()
            else:
                self.pos.next()

            # Comments!
            if self.code[start_pos.index : self.pos.index] == "//":
                while self.pos.in_range() and self.current_char() != "\n":
                    self.pos.next()
                return
            
            if self.code[start_pos.index : self.pos.index] == "/*":
                while self.pos.in_range():
                    if self.current_char() == "*":
                        self.pos.next()
                        if self.pos.in_range():
                            if self.current_char() == "/": break
                    self.pos.next()
                return

        self.tokens.append(Token(
            TokenCategory.Symbol if self.code[start_pos.index : self.pos.index] in RESERVED_IDS + RESERVED_OPS else TokenCategory.Identifier, 
            self.code[start_pos.index : self.pos.index], 
            start_pos, 
            self.pos
        ))
