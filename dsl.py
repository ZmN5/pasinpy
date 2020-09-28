
INTEGER, EOF, PLUS = 'INTEGER', 'EOF', 'PLUS'

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

        self.error()


    def expr(self):
        self.current_token = self.get_next_token()

        left = self.current_token
        self.eat(INTEGER)

        op = self.current_token
        self.eat(PLUS)

        right = self.current_token
        self.eat(INTEGER)

        return left.value + right.value
