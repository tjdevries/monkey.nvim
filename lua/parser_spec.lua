local parse = require("monkey.parser").parse

local eq = assert.are.same

describe([[Parser]], function()
  it([[can parse a let statement]], function()
    local program = parse [[
      let x = 5;
      let thePrimeagen = 420;
      return 10;
    ]]

    eq({}, program.errors)
    eq(3, #program.statements)

    local stmt = program.statements[1]
    eq("LetStatement", stmt.ty)
    eq("IdentifierExpr", stmt.name.ty)
    eq("x", stmt.name.val)
    -- eq("let x = 5;", stmt:lit())

    stmt = program.statements[2]
    eq("LetStatement", stmt.ty)
    eq("IdentifierExpr", stmt.name.ty)
    eq("thePrimeagen", stmt.name.val)

    stmt = program.statements[3]
    eq("ReturnStatement", stmt.ty)
  end)

  local check_single_expr = function(input, ty, lit)
    local program = parse(input)

    eq({}, program.errors)
    eq(1, #program.statements)

    local stmt = program.statements[1]
    eq("ExpressionStatement", stmt.ty)
    eq(ty, stmt.expr.ty)
    eq(lit, stmt:lit())
  end

  it("can parse an expr statement", function()
    check_single_expr("x;", "IdentifierExpr", "x;")
    check_single_expr("10;", "NumberExpr", "10;")
  end)

  it("can do programs too", function()
    local program = parse "x;\n10;"
    eq("x;\n10;", program:lit())
  end)

  it("can do prefiex", function()
    check_single_expr("-1;", "PrefixExpression", "-1;")
    check_single_expr("-foo;", "PrefixExpression", "-foo;")
  end)

  it("can do infix", function()
    check_single_expr("1 + 5", "InfixExpression", "(1 + 5);")
    -- check_single_expr("1 + 5 * 10 + 2", "InfixExpression", "((1 + (5 * 10)) + 2);")
  end)
end)
