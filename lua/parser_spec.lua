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
    eq("stmt-Let", stmt.ty)
    eq("expr-Identifier", stmt.name.ty)
    eq("x", stmt.name.val)
    -- eq("let x = 5;", stmt:lit())

    stmt = program.statements[2]
    eq("stmt-Let", stmt.ty)
    eq("expr-Identifier", stmt.name.ty)
    eq("thePrimeagen", stmt.name.val)

    stmt = program.statements[3]
    eq("stmt-Return", stmt.ty)
  end)

  local check_single_expr = function(input, ty, lit)
    local program = parse(input)

    eq({}, program.errors)
    eq(1, #program.statements)

    local stmt = program.statements[1]
    eq("stmt-Expression", stmt.ty)
    eq(ty, stmt.expr.ty)
    eq(lit, stmt:lit())
  end

  it("can parse an expr statement", function()
    check_single_expr("x;", "expr-Identifier", "x;")
    check_single_expr("10;", "expr-Number", "10;")
  end)

  it("can do programs too", function()
    local program = parse "x;\n10;"
    eq("x;\n10;", program:lit())
  end)

  it("can do prefiex", function()
    check_single_expr("-1;", "expr-Prefix", "-1;")
    check_single_expr("-foo;", "expr-Prefix", "-foo;")
  end)

  it("can do infix", function()
    check_single_expr("1 + 5", "expr-Infix", "(1 + 5);")
    check_single_expr("1 + 5 * 10 + 2", "expr-Infix", "((1 + (5 * 10)) + 2);")
  end)
end)
