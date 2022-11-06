local lexer = require("monkey.lexer").lex

local eq = assert.are.same

describe("lexer", function()
  it("TestNextToken", function()
    local input = "=+("

    eq({
      { ty = "Assign", lit = "=" },
      { ty = "Plus", lit = "+" },
      { ty = "LeftParen", lit = "(" },
    }, lexer(input))
  end)

  it("TestNextToken-Identifiers", function()
    local input = [[
      let five = 5;
      let add = fn(x, y) { x + y; }
      let result = add(five, five)
    ]]

    local lexed = lexer(input)
    eq({ ty = "Ident", lit = "five" }, lexed[2])
    eq({ ty = "Integer", lit = "5" }, lexed[4])
    eq({ ty = "Function", lit = "fn" }, lexed[9])
  end)

  it("TestPeeking", function()
    local lexed = lexer "== = != !"
    eq({ ty = "Eq", lit = "==" }, lexed[1])
    eq({ ty = "Assign", lit = "=" }, lexed[2])
    eq({ ty = "NotEq", lit = "!=" }, lexed[3])
    eq({ ty = "Bang", lit = "!" }, lexed[4])
  end)
end)
