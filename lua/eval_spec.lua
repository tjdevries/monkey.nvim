local parse = require("monkey.parser").parse
local eval = require("monkey.eval").eval

local eq = assert.are.same

describe("Eval", function()
  it("should evaluate a literal number", function()
    local program = parse "5"

    eq(5, eval(program).val)
  end)

  pending("should evaluate a boolean", function()
    local program = parse "true"

    eq(true, eval(program).val)
  end)
end)
