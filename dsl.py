
INTEGER, EOF, PLUS, MINUS = 'INTEGER', 'EOF', 'PLUS', 'MINUS'

class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return f'Type({self.type}, {self.value})'

    __repr__ = __str__


class Interpreter:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_token = None
        self.current_char = self.text[self.pos]

    def error(self):
        raise Exception('syntax error.')

    def advance(self):
        self.pos += 1
        if self.pos < len(self.text):
            self.current_char = self.text[self.pos]
        else:
            self.current_char = None

    def eat(self, type):
        if type == self.current_token.type:
            self.current_token = self.get_next_token()
        else:
            self.error()

    def skip_whitespaces(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def integer(self):
        anchor = self.pos
        while self.current_char is not None and self.current_char.isdigit():
            self.advance()
        return int(self.text[anchor:self.pos])


    def get_next_token(self):
        self.skip_whitespaces()

        if self.current_char is None:
            return Token(EOF, None)

        if self.current_char.isdigit():
            return Token(INTEGER, self.integer())

        if self.current_char == '+':
            self.advance()
            return Token(PLUS, self.current_char)

        if self.current_char == '-':
            self.advance()
            return Token(MINUS, self.current_char)

        self.error()

    def term(self):
        token = self.current_token
        self.eat(INTEGER)
        return token.value

    def expr(self):
        self.current_token = self.get_next_token()
        result = self.current_token.value
        self.term()
        while self.current_token.type != EOF:
            op = self.current_token
            if op.type == PLUS:
                self.eat(PLUS)
                result += self.term()
            else:
                self.eat(MINUS)
                result -= self.term()
        return result