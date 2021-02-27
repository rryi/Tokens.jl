using Tokens
using Test

function testpacked31()
    @testset "Packed31" begin
        p0_0 = Packed31(0)
        p15_257 = packed31(15%UInt8, 257%UInt32)
        
        d1 = D1"direct."
        d2 = D2"direct."
        d1identical = D1"direct."
        @test usize(d1)==7
        @test d1==d2

end