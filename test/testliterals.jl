using Tokens
using Test

"""
some elementary bounds and indexing checks with tokens constructed by Token string literals
"""
function testliterals()
    @testset "Token Literals" begin
        d1 = D1"direct."
        d2 = D2"direct."
        d1identical = D1"direct."
        @test usize(d1)==7
        @test d1==d2
        @test d1===d1identical
        @test d1!==d2
        @test category(d1) == Nibble(1)
        @test_throws BoundsError d1[8]
        @test_throws BoundsError d1[0]
        d4 = D4"äöü!"
        @test usize(d4)==7
        @test d4[5] == 'ü'
        @test d4[7] == '!'
        b1 = B1"direct."
        b2 = B2"direct."
        b2alias = b2
        b2equal = B2"direct."
        b3=  B3"longer Sampletoken äöü"
        b3equal = B3"longer Sampletoken äöü"
        @test b1==b2
        @test b1!==b2
        @test b1==d2
        @test b1!=b3
        @test b2alias === b2
        @test b2alias == b2
        @test b2equal !== b2
        @test b2equal == b2
        @test b1 !== b2
        @test b3 == b3equal
        @test b3 !== b3equal
        @test b3[24] == 'ü'
        
        h1 = H1"direct."
        h2 = H2"direct."
        h1identical = H1"direct."
        h1alias = h1
        h3 = H3"longer Sampletoken äöü"
        h3equal = H3"longer Sampletoken äöü"
        @test usize(h1)==7
        @test h1==h2
        @test h1==d2
        @test h1==b2
        @test h1===h1identical
        @test h1===h1alias
        @test h2!==h1
        @test category(h1) == Nibble(1)
        @test_throws BoundsError h1[8]
        @test_throws BoundsError h1[0]
        @test_throws BoundsError h3[0]
        @test_throws BoundsError h3[0]
        @test usize(h3) == 25
        @test h3[24] == 'ü'
        @test h3 == b3
        @test h3 !== b3
        @test_throws BoundsError h3[26]
        @test_throws BoundsError b1[8]
        @test_throws BoundsError b1[0]
    end
end       

