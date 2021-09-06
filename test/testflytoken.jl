using Tokens
using Test

function testflytoken()
    
    t1 = T_TEXT"hello world"
    @test_throws BoundsError codeunit(t1,0)
    @test_throws BoundsError codeunit(t1,12)
    @test t1 == "hello world"
    @test category(t1) == T_TEXT
    t1a = DToken(T_TEXT,"hello")
    t1b = DToken(T_TEXT,"world")
    @test t1 == t1a * " " * t1b
    @test t1 > t1a
    @test t1a < t1b
    @test t1 < t1b
    

    t2 = D1"123456"
    @test_throws BoundsError codeunit(t2,7)
    @test_throws BoundsError codeunit(t2,0)
    @test t2 == "123456"
    @test category(t2) == T_INT
    s1 = t1 * t2
    @test s1 == "hello world123456"

    d1 = D1"direct."
    d2 = D2"direct."
    d1identical = D1"direct."
    @test usize(d1)==7
    @test d1==d2
    @test d1identical === d1
end
