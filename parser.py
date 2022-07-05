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

class InvalidExportError(ParseError):
    def __init__(self, token):
        super().__init__(token)

class InvalidImportError(ParseError):
    def __init__(self, token):
        super().__init__(token)

class InvalidImportAliasError(ParseError):
    def __init__(self, token):
        super().__init__(token)

class InvalidImportDeclarationError(ParseError):
    def __init__(self, token):
        super().__init__(token)

# Nodes

class ModuleNode:
    def __init__(self, name, exports=None, imports=None, decls=None):
        self.name = name
        self.exports = exports
        self.imports = imports
        self.decls = decls

class ImportNode:
    def __init__(self, module, alias=None, ids=None):
        self.module = module
        self.alias = alias if alias is not None else self.module
        self.ids = ids

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
        name, exports, err = self.find_mod_header()
        if err is not None: return None, err

        imports, err = self.find_imports()

        module = ModuleNode(name, exports, imports, [])

        # while self.current_token().category != TC.EndOfFile:
        #     decl, err = self.get_decl()
        #     self.advance()
        
        return module
    
    def find_mod_header(self):
        # Find module header token
        while self.current_token().tup not in [(TC.Symbol, "module"), (TC.EndOfFile, None)]:
            if self.current_token().tup not in [(TC.Symbol, "module"), (TC.NewLine, None)]:
                return None, None, NoModuleHeaderError(self.current_token())
            self.advance()
        
        if self.current_token().tup == (TC.EndOfFile, None):
            return None, None, NoModuleHeaderError(self.current_token())
        
        self.advance()

        if self.current_token().category != TC.Identifier:
            return None, None, InvalidModuleNameError(self.current_token())
        
        name = self.current_token().name

        self.advance()

        if self.current_token().tup != (TC.Symbol, "("):
            return name, None, None
        
        exports = []

        while self.current_token().tup != (TC.Symbol, ")"):
            self.advance()

            if self.current_token().category != TC.Identifier:
                return None, None, InvalidExportError(self.current_token())
            
            exports.append(self.current_token().name)
            self.advance()

            if self.current_token().tup not in [(TC.Symbol, ","), (TC.Symbol, ")")]:
                return None, None, InvalidExportError(self.current_token())
        
        self.advance()

        return name, exports, None
    
    def find_imports(self):
        imports = []

        while self.current_token().tup not in [(TC.Symbol, "import"), (TC.EndOfFile, None)]:
            if self.current_token().tup not in [(TC.Symbol, "import"), (TC.NewLine, None), (TC.Indent, None)]:
                return [], None
            self.advance()

        while self.current_token().tup == (TC.Symbol, "import"):
            self.advance()
            if self.current_token().category != TC.Identifier:
                return None, InvalidImportError(self.current_token())
            
            name = self.current_token().name
            alias = None
            import_ids = []

            self.advance()

            if self.current_token().tup == (TC.Symbol, "as"):
                self.advance()
                if self.current_token().category != TC.Identifier:
                    return None, InvalidImportAliasError(self.current_token())
                alias = self.current_token().name

                self.advance()

            if self.current_token().tup == (TC.Symbol, "("):
                while self.current_token().tup != (TC.Symbol, ")"):
                    self.advance()

                    if self.current_token().category != TC.Identifier:
                        return None, InvalidImportDeclarationError(self.current_token())
                    
                    import_ids.append(self.current_token().name)
                    self.advance()

                    if self.current_token().tup not in [(TC.Symbol, ","), (TC.Symbol, ")")]:
                        return None, InvalidImportDeclarationError(self.current_token())
                
                self.advance()
            
            imports.append(ImportNode(name, alias, import_ids))
        
        return imports, None
                    

    # def get_decl(self):
    #     if self.current_token().category == TC.Identifier:
    #         pass
    #     else:
    #         pass

if __name__ == "__main__":
    import lexer
    module = Parser(lexer.Lexer("""

module Main

import StdIO as IO

    """).tokens).module

    print(module.imports[0].ids)
