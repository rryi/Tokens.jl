mutable struct TA{T}
    buf::T
end
mutable struct TS
    buf::String
end


struct S1
    i1::Int64
    i2::Int64
end

struct S2
    i1::UInt64
    i2::UInt64
end

#s1 = S1(1,2)
#s2= S2(2,3)

# error: reinterpret nicht für struct erlaubt
#s3 = reinterpret(S1,s2)


function read(t::TA{T}, pos::Int) where T
    @inbounds t.buf[pos]
end

function read(t::TS, pos::Int)
    @inbounds codeunit(t.buf,pos)
end

function tryy(size::Int, pos::Int)
    ta = TA(Vector{UInt8}(undef,size))
    ts = TS("0"^size)
    println( "array call ", read(ta,pos))
    #@code_llvm read(ta,pos)
    @code_native read(ta,pos)
    println( "string call ", read(ts,pos))
    #@code_llvm read(ts,pos)
    @code_native read(ts,pos)
end


tryy(100,7)


struct TinyToken
    bits::UInt64
    #TinyToken(i::Any) = TinyToken(0,string(i))
    function TinyToken(cat::Int,s::AbstractString)
        println("calling SE inner")
        new(cat<<56)
    end
end



tryit = TinyToken(1,"")
dump(tryit)

try2 = TinyToken(2)
dump(try2)

try2 = TinyToken(3)



@enum en ::UInt8 e1 e2 e3 e4 e5


f1(e::en) = Int(e)+7

f1(i::UInt8) = i+7

f1(i::Int) = i+7

struct Words32
         lo::UInt32
         hi::UInt32
       end
a=UInt64(0x123456789abcdef0)
b=reinterpret(Words32,a)


x = [1 2 3 4 5]
p = pointer(x)
GC.@preserve x begin
    p2 = pointer(x)
end



mutable struct PreservedDemo
    s:: String # private instance, only changed by constructor and setstring!
    p:: Ptr{UInt8}  # private instance, only changed by constructor and setstring!
    PreservedDemo(s::String) = new(s,pointer(s))
end

function setstring!(pd::PreservedDemo,s::String)
    pd.s = s
    pd.p = pointer(s)
end

function codeunit1(pd::PreservedDemo, i::Integer)
    # boundschecks ommitted to keep example short
    GC.@preserve pd unsafe_load(pd.p+(i-1))
end

function codeunit2(pd::PreservedDemo, i::Integer)
    # boundschecks ommitted to keep example short
    s = pd.s
    GC.@preserve s unsafe_load(pd.p+(i-1))
end

# Testcode
q = PreservedDemo("hello")
codeunit1(q,2) # hex 'e'
codeunit1(q,4) # hex 'l'



function needs_preserve(s::String, t::String)
    do_many_allocations() # likely to cause garbage collection
    x = s * t * "ensure Length > 0"
    p = pointer(x) # x is explicitly referenced and alive here
    c = unsafe_load(p) # x is implicitly used, not transparent to compiler
    # correct code: c = GC.preserve x unsafe_load(p)
    show(x) # x is explicitly referenced and alive here
    # code sequence suggests x is valid before and after unsafe_load(p)
    c
end


function needs_preserve_reordered_by_compiler(s::String, t::String)
    x = s * t * "ensure Length > 0"
    p = pointer(x) # x is explicitly referenced and alive hiere
    show(x) # after this, x is no longer referenced and available to garbage collection
    do_many_allocations() # likely to cause garbage collection of x
    c = unsafe_load(p) # program crash by segfault possible if x was garbagecollected
end
