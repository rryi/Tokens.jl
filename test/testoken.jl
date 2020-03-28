using Tokens
using Test

function testtoken()

    t = BufferToken(T_TEXT,"äöü")
    s = convert(SubString{String},t)
    @test s == t
end
