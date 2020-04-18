using Tokens
using Test

include("testtiny.jl")
include("testoken.jl")
include("testshared.jl")
include("testvector.jl")
include("testtree.jl")
include("testlexer.jl")
include("benchIOShared.jl")

@testset "Tokens.jl" begin
    # Write your own tests here.
    testtiny()
    testtoken()
    testshared()
    testvector()
    testtree()
    testlexer()
end
