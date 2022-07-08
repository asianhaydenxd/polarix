from lexer import TC, Token

# TODO:
#  - Implement matrices [] and lists {}
#  - Turn operator without args (i.e. "(+)") into a function
#  - Turn operator with only 1 arg (i.e. "(1 +)" or "(+ 1)") into a function
#  - Implement |=> anonymous function operator
#  - Implement pattern matching before = sign (for params and for more complex pattern matching)
#  - Parse function declarations
#  - To get infix precedences, check their declaration (look in file and in imports)
#  - Maybe allow infix declarations for regular ids for use without <> brackets?
#  - Implement data (and type, i.e. type synonym) keywords
#  - Parse imperative code

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

class UnexpectedIndentError(ParseError):
    def __init__(self, token):
        super().__init__(token)

class DeclarationExpectedError(ParseError):
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


class FactorIdNode:
    def __init__(self, id):
        self.id = id

class FunctionNode:
    def __init__(self, name, arg):
        self.name = name
        self.arg = arg

class BinaryOperatorNode:
    def __init__(self, name, left, right):
        self.name = name
        self.left = left
        self.right = right

class TupleNode:
    def __init__(self, members):
        self.members = members
        
# class MatrixNode:
#     def __init__(self, matrix):
#         self.matrix = matrix

class ListNode:
    def __init__(self, seq):
        self.list = seq

# Parser

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

        while self.current_token().category != TC.EndOfFile:
            decl, err = self.get_decl()
            if err is not None: return None, err
            if decl is not None: module.decls.append(decl)
            self.advance()
        
        return module, None
    
    def get_decl(self):
        if self.current_token().tup == (TC.NewLine, None):
            return None, None
        if self.current_token().tup == (TC.Indent, None):
            self.advance()
            if self.current_token().tup in [(TC.NewLine, None), (TC.EndOfFile, None)]: # Do nothing if the tabbed line is empty
                self.index -= 1
                return None, None
            return None, UnexpectedIndentError(self.current_token())
        if self.current_token().category == TC.Identifier:
            return self.get_function()
        if self.current_token().tup == (TC.Symbol, "data"):
            return self.get_data()
        return None, DeclarationExpectedError(self.current_token())
    
    def get_function(self):
        self.advance()
        if self.current_token().tup == (TC.Symbol, ":"):
            return
        if self.current_token().tup == (TC.Symbol, "="): # Change to take in pattern matching
            self.advance()
            return self.parse_function()
    
    def parse_function(self):
        return self.tup_op()

    def tup_op(self):
        left, err = self.bin_op()
        if self.current_token().tup != (TC.Symbol, ","):
            return left, err
        left = [left]
        while self.current_token().tup == (TC.Symbol, ","):
            self.advance()
            right, err = self.bin_op()
            left.append(right)
        return TupleNode(left), err
    
    def bin_op(self, precedence=0):
        left, err = self.function_op() if precedence == 0 else self.bin_op(precedence-1)
        if self.current_token().category == TC.Operator:
            optoken = self.current_token()
            self.advance()
            right, err = self.function_op() if precedence == 0 else self.bin_op(precedence-1)
            left = BinaryOperatorNode(optoken, left, right)
        return left, err

    def function_op(self):
        init, err = self.factor()
        arg, err = self.factor()
        if arg is None or init.id.category != TC.Identifier: return init, err
        return FunctionNode(init, arg), err

    def factor(self):
        if self.current_token().tup == (TC.Symbol, "("):
            self.advance()
            expr, err = self.parse_function()
            if self.current_token().tup == (TC.Symbol, ")"):
                self.advance()
            if err is not None: return None, err
            return expr, None
        
        # List
        if self.current_token().tup == (TC.Symbol, "{"):
            self.advance()
            expr, err = self.parse_function()
            if self.current_token().tup == (TC.Symbol, "}"):
                self.advance()
            if err is not None: return None, err
            # Turn tuple (or single element) into list
            return (ListNode(expr.members if type(expr) == TupleNode else [expr])), None
        
        if self.current_token().category in [TC.Identifier, TC.String, TC.Character, TC.Number]:
            tok = self.current_token()
            self.advance()
            return FactorIdNode(tok), None

        return None, None

    def get_data(self):
        pass
    
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

def printmodule(module):
    print(f"- name: {module.name}")
    print("  exports:")
    for exported in module.exports:
        print(f"  - {exported}")
    print("  imports:")
    for imported in module.imports:
        print(f"  - {imported.alias}")
        if imported.alias != imported.module:
            print(f"    name: {imported.module}")
        if imported.ids:
            print("    ids:")
        for id in imported.ids:
            print(f"     - {id}")
    print(f"  declarations:")
    for decl in module.decls:
        print(printdecl(decl, 2))

def printdecl(decl, indent):
    if type(decl) == Token:
        return " " * (indent*2) + str(decl) + "\n"
    if type(decl) == FactorIdNode:
        return " " * (indent*2) + str(decl.id) + "\n"
    if type(decl) == FunctionNode:
        return " " * (indent*2) + "name:\n" + printdecl(decl.name, indent+1) \
             + " " * (indent*2) + "arg:\n" + printdecl(decl.arg, indent+1)
    if type(decl) == BinaryOperatorNode:
        return " " * (indent*2) + "name:\n" + printdecl(decl.name, indent+1) \
             + " " * (indent*2) + "left:\n" + printdecl(decl.left, indent+1) \
             + " " * (indent*2) + "right:\n" + printdecl(decl.right, indent+1)
    if type(decl) == TupleNode:
        return " " * (indent*2) + "members:\n" + "".join([printdecl(member, indent+1) for member in decl.members])
    if type(decl) == ListNode:
        return " " * (indent*2) + "list members:\n" + "".join([printdecl(member, indent+1) for member in decl.list])
