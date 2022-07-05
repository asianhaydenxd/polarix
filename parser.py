from lexer import TC

# Parse Errors

class ParseError:
    def __init__(self, token):
        self.token = token

class NoModuleHeaderError(ParseError):
    def __init__(self, token):
        super().__init__(token)

class InvalidModuleNameError(ParseError):
    def __init__(self, token):
        super().__init__(token)

# Nodes

class ModuleNode:
    def __init__(self, name, decls):
        self.name = name
        self.decls = decls

class DeclarationNode:
    def __init__(self, name, node):
        self.name = name
        self.node = node

class DefinitionNode:
    def __init__(self, match_node, func_node):
        self.match_node = match_node
        self.func_node = func_node

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.index = 0
        self.module = self.parse()
    
    def current_token(self):
        return self.tokens[self.index]

    def advance(self):
        self.index += 1

    def parse(self):
        name, err = self.find_mod_header()
    
    def find_mod_header(self):
        # Find module header token
        while self.current_token().tup not in [(TC.Symbol, "module"), (TC.EndOfFile, None)]:
            if self.current_token().tup not in [(TC.Symbol, "module"), (TC.NewLine, None)]:
                return None, NoModuleHeaderError(self.current_token())
            self.advance()
        
        if self.current_token().tup == (TC.EndOfFile, None):
            return None, NoModuleHeaderError(self.current_token())
        
        self.advance()

        if self.current_token().category != TC.Identifier:
            return None, InvalidModuleNameError(self.current_token())
        
        return self.current_token().name, None
