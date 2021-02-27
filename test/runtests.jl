using Tokens
using Test

include("testpacked31.jl")
include("testliterals.jl")
include("testflytoken.jl")
include("testtoken.jl")
include("testioshared.jl")
include("testvector.jl")
include("testtree.jl")
include("testlexer.jl")

@testset "Tokens.jl" begin
    # Write your own tests here.
    testpacked31()
    testliterals()
    testflytoken()
    testtoken()
    testshared()
    testvector()
    testtree()
    testlexer()
end
