using Tokens
using Test

function testpacked31()
    @testset "Packed31" begin
        p0_0 = Packed31(0%UInt32)
        p15_255 = Packed31(15%UInt8, 255%UInt32)        
        p15_256 = Packed31(15%UInt8, 256%UInt32)
        p15_257 = Packed31(15%UInt8, 257%UInt32)
        @test p0_0.nibble == Nibble(0)
        @test p15_255.nibble == Nibble(15)        
        @test p15_256.nibble == Nibble(15)        
        @test p15_257.nibble == Nibble(15)
        @test p15_255.len == 255            
        @test p15_256.len == 256           
        @test p15_257.len == 257        
        p15_max = Packed31(15%UInt8,UInt32(2^27 - 1))      
        @test UInt32(p15_max) == UInt32(2^31 - 1)
        
        @test p0_0.nibble == Nibble(0)
        @test p0_0.nibble == Nibble(0)
        

    end
end