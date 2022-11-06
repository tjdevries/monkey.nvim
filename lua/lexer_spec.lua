local lexer = require("monkey.lexer").lex

local eq = assert.are.same

describe("lexer", function()
  it("TestNextToken", function()
    local input = "=+("

    eq({
      { ty = "lex-Assign", lit = "=" },
      { ty = "lex-Plus", lit = "+" },
      { ty = "lex-LeftParen", lit = "(" },
    }, lexer(input))
  end)

  it("TestNextToken-Identifiers", function()
    local input = [[
      let five = 5;
      let add = fn(x, y) { x + y; }
      let result = add(five, five)
    ]]

    local lexed = lexer(input)
    eq({ ty = "lex-Identifier", lit = "five" }, lexed[2])
    eq({ ty = "lex-Integer", lit = "5" }, lexed[4])
    eq({ ty = "lex-Function", lit = "fn" }, lexed[9])
  end)

  it("TestPeeking", function()
    local lexed = lexer "== = != !"
    eq({ ty = "lex-Eq", lit = "==" }, lexed[1])
    eq({ ty = "lex-Assign", lit = "=" }, lexed[2])
    eq({ ty = "lex-NotEq", lit = "!=" }, lexed[3])
    eq({ ty = "lex-Bang", lit = "!" }, lexed[4])
  end)
end)
