local parse = require("monkey.parser").parse
local eval = require("monkey.eval").eval

local eq = assert.are.same

describe("Eval", function()
  it("should evaluate a literal number", function()
    local program = parse "5"

    eq(5, eval(program).val)
  end)

  it("should evaluate a boolean", function()
    local program = parse "!!true"
    eq("(!(!true));", program:lit())
    -- eq({}, program.statements[1])

    eq(true, eval(program).val)
  end)

  it("evaluated grouped expresssion", function()
    local program = parse "(5 + 10);"

    eq({}, program.errors)
    eq(15, eval(program).val)
  end)

  it("evaluated grouped expresssion", function()
    local program = parse "(5 + 10) * 5;"

    eq({}, program.errors)
    eq(75, eval(program).val)
  end)
end)
