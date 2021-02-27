using Tokens
using Test

function testflytoken()
    t1 = T_TEXT"hello world"
    @test_throws BoundsError codeunit(t1,0)
    @test_throws BoundsError codeunit(t1,12)
    @test t1 == "hello world"
    @test category(t1) == T_TEXT
    t1a = DirectToken(T_TEXT,"hello")
    t1b = DirectToken(T_TEXT," world")
    @test t1 == t1a * t1b
    @test t1 > t1a
    @test t1 < t2a
    t1c =

    t2 = T_INT"123456"
    @test_throws BoundsError codeunit(t2,7)
    @test_throws BoundsError codeunit(t2,0)
    @test t2 == "123456"
    @test category(t2) == T_INT
    s1 = t1 * t2
    @test s1 == "hello world123456"

end
