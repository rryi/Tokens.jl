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


struct FlyToken
    bits::UInt64
    #FlyToken(i::Any) = FlyToken(0,string(i))
    function FlyToken(cat::Int,s::AbstractString)
        println("calling SE inner")
        new(cat<<56)
    end
end



tryit = FlyToken(1,"")
dump(tryit)

try2 = FlyToken(2)
dump(try2)

try2 = FlyToken(3)



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

# # Chapter 1

# ## Using Types
using BenchmarkTools

# Declare type of function argument
iam(x::Integer) = "an integer"
iam(x::String) = "a string"

function addme(a, b)
  #Declare type of local variable x
  x::Int64 = 2
  #Type of variable y will be inferred
  y = (a+b) / x
  return y
end

iam(1)

iam("1")

iam(1.5)

# ## Multiple Dispatch
sumsqr(x, y) = x^2 + y^2

sumsqr(1, 2)

sumsqr(1.5, 2.5)

sumsqr(1 + 2im , 2 + 3im)

sumsqr(2 + 2im, 2.5)

#Composite Types

struct Pixel
    x::Int64
    y::Int64
    color::Int64
end

p = Pixel(5,5, 100)

p.x = 10;

p.x

# immutable types

mutable struct MPixel
    x::Int64
    y::Int64
    color::Int64
end

p = MPixel(5,5, 100)

p.x=10;

p.x


# TYpe Parameters

struct PPixel{T}
    x::Int64
    y::Int64
    color::T
end

#Type Inference

[x for x=1:5]

# Type Stability

function pos(x)
   if x < 0
      return 0
   else
      return x
   end
end

pos(-1)


pos(-2.5)


pos(2.5)


typeof(pos(2.5))


typeof(pos(-2.5))

function pos_fixed(x)
    if x < 0
        return zero(x)
    else
        return x
    end
end

pos_fixed(-2.4)

pos_fixed(-2)

typeof(pos_fixed(-2.4))

typeof(pos_fixed(-2))

@btime pos(2.5)
@btime pos_fixed(2.5)

@code_warntype pos(2.5)
@code_warntype pos_fixed(2.5)
@code_llvm pos(2.5)
@code_llvm pos_fixed(2.5)

@code_native pos(2.5)
@code_native pos_fixed(2.5)

# Loop variables

function sumsqrtn(n)
    r = 0
    for i = 1:n
        r = r + sqrt(i)
    end
    return r
end


@code_warntype sumsqrtn(5)

function sumsqrtn_fixed(n)
     r = 0.0
     for i = 1:n
         r = r + sqrt(i)
     end
     return r
end

@code_warntype sumsqrtn_fixed(5)

@btime sumsqrtn(1000_000)

@btime sumsqrtn_fixed(1000_000)

#Kernel Methods

function string_zeros(s::AbstractString)
    n=1000_000
    x = s=="Int64" ?
        Vector{Int64}(undef,n) :
        Vector{Float64}(undef, n)
    for i in 1:length(x)
        x[i] = 0
    end
    return x
end

@btime string_zeros("Int64");


function string_zeros_stable(s::AbstractString)
    n = 1000_000
    x = s=="Int64" ?
        Vector{Int64}(undef, n) :
        Vector{Float64}(undef, n)
    return fill_zeros(x)
end

function fill_zeros(x)
    for i in 1:length(x)
        x[i] = 0
    end
    return x
end

@btime string_zeros_stable("Int64");

#Types in storage
## Arrays

a = Int64[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
b = Number[1,2,3,4,5,6,7,8,9,10]

function arr_sumsqr(x::Array{T}) where T <: Number
    r = zero(T)
    for i = 1:length(x)
        r = r + x[i] ^ 2
    end
    return r
end

 @btime arr_sumsqr(a)
 @btime arr_sumsqr(b)

## Composite Types
struct Point
    x
    y
end

struct ConcretePoint
    x::Float64
    y::Float64
end


function sumsqr_points(a)
    s=0.0
    for x in a
        s = s + x.x^2 + x.y^2
    end
    return s
end

p_array = [Point(rand(), rand()) for i in 1:1000_000];
cp_array = [ConcretePoint(rand(), rand()) for i in 1:1000_000];

@btime sumsqr_points(p_array)
@btime sumsqr_points(cp_array)

## Parametric Composite Types

struct PointWithAbstract
    x::AbstractFloat
    y::AbstractFloat
end

struct ParametricPoint{T <: AbstractFloat}
    x::T
    y::T
end

pp_array = [ParametricPoint(rand(), rand()) for i in 1:1000_000];


@btime sumsqr_points(pp_array)
