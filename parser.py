# Parse Errors

class ParseError:
    def __init__(self, token):
        self.token = token

class NoModuleHeaderError(ParseError):
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
