##############################################################
#                      Lexer
##############################################################
from collections import OrderedDict

INTEGER, EOF, MUL, DIV = 'INTEGER', 'EOF', 'MUL', 'DIV'

PLUS, MINUS = 'PLUS', 'MINUS'
MUL, INTEGER_DIV, FLOAT_DIV, DIV = 'MUL', 'INTEGER_DIV', 'FLOAT_DIV', 'DIV'
LPAREN, RPAREN = 'LPAREN', 'RPAREN'
PROGRAM, BEGIN, END = 'PROGRAM', 'BEGIN', 'END'
ASSIGN, ID = 'ASSIGN', 'ID'
COMMA, COLON, SEMI, DOT = 'COMMA', 'COLON', 'SEMI', 'DOT'
REAL_CONST, INTEGER_CONST = 'REAL_CONST', 'INTEGER_CONST'
VAR, REAL = 'VAR', 'REAL'

class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return f'Type({self.type}, {self.value})'

    __repr__ = __str__


RESERVED_KEYWORDS = dict(
    BEGIN=Token(BEGIN, BEGIN),
    END=Token(END, END),
    PROGRAM=Token(PROGRAM, PROGRAM),
    VAR=Token(VAR, VAR),
    DIV=Token(INTEGER_DIV, DIV),
    INTEGER=Token(INTEGER, INTEGER),
    REAL=Token(REAL, REAL),
)


class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]
        self.current_token = None

    def error(self):
        raise Exception('syntax error.')

    def peek(self):
        if self.pos >= len(self.text) - 1:
            return None
        return self.text[self.pos + 1]

    def advance(self):
        self.pos += 1
        if self.pos < len(self.text):
            self.current_char = self.text[self.pos]
        else:
            self.current_char = None

    def number(self):
        anchor = self.pos
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        if self.current_char == '.':
            result += self.current_char
            self.advance()
            while self.current_char is not None and self.current_char.isdigit():
                result += self.current_char
                self.advance()
            token = Token(REAL_CONST, float(result))
        else:
            token = Token(INTEGER_CONST, int(result))
        return token

    def skip_comment(self):
        while self.current_char != '}':
            self.advance()
        self.advance()

    def skip_whitespaces(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def _id(self):
        result = ''
        while self.current_char is not None and self.current_char.isalnum():
            result += self.current_char
            self.advance()
        token = RESERVED_KEYWORDS.get(result, Token(ID, result))
        return token

    def get_next_token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespaces()
                continue

            if self.current_char == '{':
                self.advance()
                self.skip_comment()
                continue

            if self.current_char.isdigit():
                return self.number()

            if self.current_char.isalpha():
                return self._id()

            if self.current_char == ',':
                self.advance()
                return Token(COMMA, ',')

            if self.current_char == '/':
                self.advance()
                return Token(FLOAT_DIV, '/')

            if self.current_char == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(ASSIGN, ':=')

            if self.current_char == ':':
                self.advance()
                return Token(COLON, ':')

            if self.current_char == ';':
                self.advance()
                return Token(SEMI, ';')

            if self.current_char == '.':
                self.advance()
                return Token(DOT, '.')

            if self.current_char == '*':
                self.advance()
                return Token(MUL, '*')

            if self.current_char == '/':
                self.advance()
                return Token(DIV, '/')

            if self.current_char == '+':
                self.advance()
                return Token(PLUS, '+')

            if self.current_char == '-':
                self.advance()
                return Token(MINUS, '-')

            if self.current_char == '(':
                self.advance()
                return Token(LPAREN, '(')

            if self.current_char == ')':
                self.advance()
                return Token(RPAREN, ')')
            self.error()

        return Token(EOF, None)


##############################################################
#                      Parser
##############################################################


class AST:
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = op
        self.op = op
        self.right = right


class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Compound(AST):
    def __init__(self):
        self.children = []


class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.right = right
        self.token = self.op = op


class Var(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class NoOp(AST):
    def __init__(self):
        pass


class Program(AST):
    def __init__(self, name, block):
        self.name = name
        self.block = block


class Block(AST):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('parse error.')

    def eat(self, token_type):
        if token_type == self.current_token.type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def program(self):
        self.eat(PROGRAM)
        var_node = self.variable()
        prog_name = var_node.value
        self.eat(SEMI)
        block_node = self.block()
        program_node = Program(prog_name, block_node)
        self.eat(DOT)
        return program_node

    def block(self):
        declarations = self.declarations()
        compound_statement = self.compound_statement()
        return Block(declarations, compound_statement)

    def declarations(self):
        result = []
        if self.current_token.type == VAR:
            self.eat(VAR)
            while self.current_token.type == ID:
                var_decl = self.variable_declaration()
                result.extend(var_decl)
                self.eat(SEMI)
        return result

    def variable_declaration(self):
        var_nodes = [Var(self.current_token)]
        self.eat(ID)
        while self.current_token.type == COMMA:
            self.eat(COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(ID)
        self.eat(COLON)
        type_node = self.type_spec()
        var_decls = [VarDecl(node, type_node) for node in var_nodes]
        return var_decls

    def type_spec(self):
        token = self.current_token
        if token.type == INTEGER:
            self.eat(INTEGER)
        else:
            self.eat(REAL)
        node = Type(token)
        return node

    def compound_statement(self):
        self.eat(BEGIN)
        nodes = self.statement_list()
        self.eat(END)
        root = Compound()
        for node in nodes:
            root.children.append(node)
        return root

    def statement_list(self):
        node = self.statement()
        results = [node]
        while self.current_token.type == SEMI:
            self.eat(SEMI)
            results.append(self.statement())

        if self.current_token.type == ID:
            self.error()
        return results

    def statement(self):
        if self.current_token.type == BEGIN:
            return self.compound_statement()
        elif self.current_token.type == ID:
            return self.assign_statement()
        else:
            return self.empty()

    def assign_statement(self):
        left = self.variable()
        token = self.current_token
        self.eat(ASSIGN)
        right = self.expr()
        return Assign(left, token, right)

    def variable(self):
        node = Var(self.current_token)
        self.eat(ID)
        return node

    def empty(self):
        return NoOp()

    def expr(self):
        node = self.term()
        while self.current_token.type in (PLUS, MINUS):
            token = self.current_token
            if token.type == PLUS:
                self.eat(PLUS)
            elif token.type == MINUS:
                self.eat(MINUS)
            node = BinOp(node, token, self.term())
        return node

    def term(self):
        node = self.factor()
        while self.current_token.type in (MUL, INTEGER_DIV, FLOAT_DIV):
            token = self.current_token
            if token.type == MUL:
                self.eat(MUL)
            elif token.type == INTEGER_DIV:
                self.eat(INTEGER_DIV)
            elif token.type == FLOAT_DIV:
                self.eat(FLOAT_DIV)
            node = BinOp(node, token, self.factor())
        return node

    def factor(self):
        token = self.current_token
        if token.type == PLUS:
            self.eat(PLUS)
            return UnaryOp(token, self.factor())
        elif token.type == MINUS:
            self.eat(MINUS)
            return UnaryOp(token, self.factor())
        elif token.type == INTEGER_CONST:
            self.eat(INTEGER_CONST)
            return Num(token)
        elif token.type == REAL_CONST:
            self.eat(REAL_CONST)
            return Num(token)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr()
            self.eat(RPAREN)
            return node
        else:
            node = self.variable()
            return node

    def parse(self):
        node = self.program()
        if self.current_token.type != EOF:
            self.error()
        return node


##############################################################
#                       Interpreter
##############################################################


class NodeVisitor:
    def visit(self, node):
        method_name = f'visit{type(node).__name__}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception(f'no visitor for this node: {node}.')


class Interpreter(NodeVisitor):
    def __init__(self, tree):
        self.tree = tree
        self.GLOBAL_SCOPE = {}

    def visitProgram(self, node):
        self.visit(node.block)

    def visitBlock(self, node):
        for decl in node.declarations:
            self.visit(decl)
        self.visit(node.compound_statement)

    def visitVarDecl(self, node):
        pass

    def visitType(self, node):
        pass

    def visitNum(self, node):
        return node.value

    def visitBinOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        if node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        if node.op.type == INTEGER_DIV:
            return self.visit(node.left) // self.visit(node.right)
        if node.op.type == FLOAT_DIV:
            return float(self.visit(node.left)) / float(self.visit(node.right))
        if node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)

    def visitUnaryOp(self, node):
        if node.op.type == PLUS:
            return +self.visit(node.expr)
        elif node.op.type == MINUS:
            return -self.visit(node.expr)

    def visitCompound(self, node):
        for child in node.children:
            self.visit(child)

    def visitNoOp(self, node):
        pass

    def visitVar(self, node):
        var_name = node.value
        val = self.GLOBAL_SCOPE.get(var_name)
        if val is None:
            raise NameError(repr(var_name))
        return val

    def visitAssign(self, node):
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def interpret(self):
        return self.visit(self.tree)


##############################################################
#                  Symbols and Symbol Table
##############################################################


class Symbol:
    def __init__(self, name, type=None):
        self.name = name
        self.type = type

class BuiltinTypeSymbol(Symbol):
    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    __repr__ = __str__


class VarSysmbol(Symbol):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return f'<{self.name}:{self.type}>'

    __repr__ = __str__


class SymbolTable:
    def __init__(self):
        self._symbols = OrderedDict()
        self._init_builtin_types()

    def define(self, symbol):
        print(f'define symbol {symbol}')
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        print(f'lookup {name}')
        return self._symbols.get(name)

    def _init_builtin_types(self):
        self.define(BuiltinTypeSymbol('INTEGER'))
        self.define(BuiltinTypeSymbol('REAL'))


class SymbolTableBuilder(NodeVisitor):
    def __init__(self):
        self._symtab = SymbolTable()

    def visitNum(self, node):
        pass

    def visitBinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visitUnaryOp(self, node):
        self.visit(self.expr)

    def visitCompound(self, node):
        for child in node.children:
            self.visit(child)

    def visitAssign(self, node):
        var_name = node.left.value
        var_sym = self._symtab.lookup(var_name)
        if not var_sym:
            raise NameError(repr(var_name))
        self.visit(node.right)

    def visitVar(self, node):
        var_name = node.value
        var_sym = self._symtab.lookup(var_name)
        if not var_sym:
            raise NameError(repr(var_name))

    def visitNoOp(self, node):
        pass

    def visitProgram(self, node):
        self.visit(node.block)

    def visitBlock(self, node):
        for decl in node.declarations:
            self.visit(decl)
        self.visit(node.compound_statement)

    def visitVarDecl(self, node):
        type_node = node.type_node
        type_sym = self._symtab.lookup(type_node.value)
        var_name = node.var_node.value
        var_sym = VarSysmbol(var_name, type_sym)
        self._symtab.define(var_sym)

    def visitType(self):
        pass


def test(text):
    lexer = Lexer(text)
    parser = Parser(lexer)
    tree = parser.parse()

    symtab_builder = SymbolTableBuilder()
    symtab_builder.visit(tree)

    interpreter = Interpreter(tree)
    interpreter.interpret()
    print(interpreter.GLOBAL_SCOPE)

