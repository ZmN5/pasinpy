
INTEGER, EOF, MUL, DIV = 'INTEGER', 'EOF', 'MUL', 'DIV'

PLUS, MINUS = 'PLUS', 'MINUS'
MUL, DIV = 'MUL', 'DIV'
LPAREN, RPAREN = 'LPAREN', 'RPAREN'

class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return f'Type({self.type}, {self.value})'

    __repr__ = __str__


class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]
        self.current_token = None

    def error(self):
        raise Exception('syntax error.')

    def advance(self):
        self.pos += 1
        if self.pos < len(self.text):
            self.current_char = self.text[self.pos]
        else:
            self.current_char = None

    def integer(self):
        anchor = self.pos
        while self.current_char is not None and self.current_char.isdigit():
            self.advance()
        return int(self.text[anchor:self.pos])

    def skip_whitespaces(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def get_next_token(self):
        self.skip_whitespaces()

        if self.current_char is None:
            return Token(EOF, None)

        if self.current_char.isdigit():
            return Token(INTEGER, self.integer())

        if self.current_char == '*':
            self.advance()
            return Token(MUL, self.current_char)

        if self.current_char == '/':
            self.advance()
            return Token(DIV, self.current_char)

        if self.current_char == '+':
            self.advance()
            return Token(PLUS, self.current_char)

        if self.current_char == '-':
            self.advance()
            return Token(MINUS, self.current_char)

        if self.current_char == '(':
            self.advance()
            return Token(LPAREN, self.current_char)

        if self.current_char == ')':
            self.advance()
            return Token(RPAREN, self.current_char)

        self.error()


class Interpreter:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def eat(self, token_type):
        if token_type == self.current_token.type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def factor(self):
        token = self.current_token
        if token.type == INTEGER:
            self.eat(INTEGER)
            return token.value
        elif token.type == LPAREN:
            self.eat(LPAREN)
            result = self.expr()
            self.eat(RPAREN)
            return result

    def term(self):
        result = self.factor()
        while self.current_token.type in (MUL, DIV):
            op = self.current_token
            if op.type == MUL:
                self.eat(MUL)
                result *= self.factor()
            else:
                self.eat(DIV)
                result //= self.factor()
        return result

    def expr(self):
        result = self.term()
        while self.current_token.type in (PLUS, MINUS):
            op = self.current_token
            if op.type == PLUS:
                self.eat(PLUS)
                result += self.term()
            else:
                self.eat(MINUS)
                result -= self.term()
        return result
