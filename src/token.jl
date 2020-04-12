
"""
Central token type: a flyweight token plus its buffer
"""
struct GenericToken{FLY<:FlyToken} <: AbstractToken
    fly :: FLY # category, size, offset, possibly content
    buffer :: String # memory with token text data or EMPTYSTRING
    function GenericToken{FLY}(fly::FLY, buffer::String)
        @boundscheck isdirect(fly) || usize(fly)==0 || checkbounds(buffer,offset(fly)+usize(fly))
        new(fly,buffer)
    end
end

## GenericToken constructors

Base.@propagate_inbounds
function GenericToken(fly::FLY, buffer::String) where FLY<:FlyToken
    GenericToken{FLY}(fly,buffer)
end


"""
Recommended general-purpose Token implementation.

Immutable token, contents either directly stored or buffer based.

# implementation details

Constructors try to avoid copying contents: if contents is backed by a
String instance, that string will be used as buffer via a BufferFly,
even if size is below 8.
"""
const Token = GenericToken{HybridFly}


"""
Immutable token, always buffer based

Use this type if you expect most token sizes to be above 7
"""
const BToken = GenericToken{BufferFly}

 Token(t::DirectFly) = @inbounds Token(hf(t),EMPTYSTRING)
BToken(t::DirectFly) = BToken(BufferFly(category(t),usize(t)),string(t))
 Token(t::BToken) = @inbounds Token(hf(t.fly),t.buffer)
BToken(t::BToken) = t
 Token(t::Token) = t
BToken(t::Token) = isdirect(t) ? BToken(df(t.fly)) : BToken(bf(t.fly),t.buffer)

 Token(cat::TCategory, s::String) = Token(HybridFly(cat,usize(s)),s)
BToken(cat::TCategory, s::String) = BToken(BufferFly(cat,usize(s)),s)
 Token(cat::TCategory) = Token(cat,EMPTYSTRING)
BToken(cat::TCategory) = BToken(cat,EMPTYSTRING)

Base.@propagate_inbounds
function BToken(cat::TCategory,offset::UInt32, size::UInt64, s::String)
    size == 0 && return BToken(cat)
    @boundscheck checkbounds(s,offset+size)
    BToken(BufferFly(cat,offset,size),s)
end
Base.@propagate_inbounds
function Token(cat::TCategory,offset::UInt32, size::UInt64, s::String)
    size == 0 && return Token(cat)
    @boundscheck checkbounds(s,offset+size)
    Token(HybridFly(cat,offset,size),s)
end

function BToken(cat::TCategory,s::SubString{String})
    BToken(BufferFly(cat,UInt32(s.offset),s.ncodeunits%UInt64),s.string)
end
function Token(cat::TCategory,s::SubString{String})
    Token(HybridFly(cat,UInt32(s.offset),s.ncodeunits%UInt64),s.string)
end

BToken(cat::TCategory,s::AbstractString) = BToken(cat,string(s))
 Token(cat::TCategory,s::AbstractString) = Token(cat,string(s))
DirectFly(t::Token) = isdirect(t) ? df(t.fly) : DirectFly(category(t),t)

#= not used - always reference string  if not constructed from Token or IOShared
Base.@propagate_inbounds function Token(
    cat::TCategory,offset::UInt32, size::UInt64, s::String, direct::Bool=false)
    if direct && (size<=MAX_DIRECT_SIZE)
        new(hf(DirectFly(cat,offset,size,s)),EMPTYSTRING)
    else
        @boundscheck checkbounds(s,offset+size)
        new(hf(BufferFly(cat,offset,size)),s)
    end
end

Base.@propagate_inbounds function Token(cat::TCategory, s::Utf8String, direct::Bool=false)
    size = s.ncodeunits
    if direct && (size <= MAX_DIRECT_SIZE)
        Token(hf(DirectFly(cat,s)),EMPTYSTRING)
    else
        Token(BToken(cat,s))
#        @boundscheck checksize(size,MAX_TOKEN_SIZE)
#        @inbounds Token(hf(BufferFly(cat,s.offset,size)),s.string)
    end
end
=#



## Token API methods ##

offset(t::GenericToken) = offset(t.fly)

category(t::GenericToken) = category(t.fly)

isdirect(t::GenericToken) = isdirect(t.fly)

usize(t::GenericToken) = usize(t.fly)


## Base methods for tokens ##

Base.cmp(a::Token, b::DirectFly) = isdirect(a.fly) ? cmp(a.fly,b) : cmp_codeunits(a,b)

Base.cmp(a::DirectFly, b::Token) = isdirect(b.fly) ? cmp(a,b.fly) : cmp_codeunits(a,b)

Base.cmp(a::Token, b::Token) = reinterpret(Int64,u64(a.fly)|u64(b.fly))>=0 ? cmp(a.fly,b.fly) : cmp_codeunits(a,b)


## converts

Base.convert(::Type{GenericToken{FLY}}, s::AbstractString) where FLY <:FlyToken = GenericToken{FLY}(T_TEXT,s)

Base.convert(::Type{GenericToken{FLY}}, t::AbstractToken) where FLY <:FlyToken = GenericToken{FLY}(t)



# any use case?? better work with tokens directly, do not convert to (Sub)String
Base.convert(::Type{SubString{String}}, t::BToken) =
    @inbounds SubString(t.buffer,offset(t)+1,thisind(offset(t)+ncodeunits(t)))


function Base.codeunit(t::GenericToken, i::Int)
    if isdirect(t.fly) # optimized away for BToken because isdirect is constantly false
        codeunit(df(t.fly),i)
    else
        @boundscheck checkbounds(t, i)
        @inbounds return codeunit(t.buffer, offset(bf(t.fly)) + i)
    end
end


function Base.hash(t::BToken, h::UInt)
    h += base.memhash_seed
    # note: use pointer(s) here (see #6058).
    s = t.buffer
    GC.@preserve s ccall(memhash, UInt, (Ptr{UInt8}, Csize_t, UInt32), pointer(s)+offset(t.fly), sizeof(t.fly), h % UInt32) + h
end


function Base.hash(t::Token, h::UInt)
    isdirect(t) && return hash(df(t.fly),h)
    hash(BToken(t),h)
end


# any advantage over default impl?? TODO -> look at generated default code
# try & test with default impl!
#=
function Base.iterate(t::GenericToken, i::Integer=firstindex(s))
    i == ncodeunits(t)+1 && return nothing
    @boundscheck checkbounds(t, i)
    y = iterate(t.buffer, t.offset + i)
    y === nothing && return nothing
    c, i = y
    return c, i - t.offset
end

function Base.getindex(t::BToken, i::Integer)
    @boundscheck checkbounds(t, i)
    @inbounds return getindex(t.buffer, offset(t) + i)
end

function Base.getindex(t::Token, i::Integer)
    @boundscheck checkbounds(t, i)
    @inbounds if isdirect(t)
        getindex(df(t.fly),i)
    else
        getindex(t.buffer, offset(bf(t.fly)) + i)
    end
end
=#


function Base.read(io::IO, ::Type{Token})
    t = read(io,HybridFly)
    size = usize(t)
    if size <= MAX_DIRECT_SIZE
        t = hf(read(io,df(t)))
        @inbounds Token(t,EMPTYSTRING)
    else
        @inbounds Token(t, read(io,size,String))
    end
end


function Base.read(io::IO, ::Type{BToken})
    t = read(io,HybridFly)
    @inbounds BToken(bf(t), read(io,size,String))
end


# serialize Token and BToken
function Base.write(io::IO, t::GenericToken)
    tt = t.fly
    if isdirect(tt) # optimized away for BufferFly
        write(io,df(tt))
    else
        write(io,hf(tt))
        write(io,offset(bf(tt)),usize(bf(tt)),t.buffer)
    end
end



#=

String(s::FlyToken) = String(reinterpret(UInt8, [s.size_content|>ntoh])[1:sizeof(s)])

Base.lastindex(s::FlyToken) = Int(s.size_content & 0xf)
Base.iterate(s::FlyToken, i::Integer) = iterate(String(s), i)
Base.iterate(s::FlyToken) = iterate(String(s))
Base.sizeof(s::FlyToken) = Int(s.size_content & 0xf)
Base.print(s::FlyToken) = print(String(s))
Base.display(s::FlyToken) = display(String(s))
Base.convert(::FlyToken{T}, s::String) where T = FlyToken{T}(s)
Base.convert(::String, ss::FlyToken) = String(a) #reduce(*, ss)
Base.firstindex(::FlyToken) = 1
Base.ncodeunits(s::FlyToken) = ncodeunits(String(s))
Base.codeunit(s::FlyToken, i) = codeunits(String(s), i)
Base.isvalid(s::FlyToken, i::Integer) = isvalid(String(s), i)

Base.getindex(s::FlyToken{T}, i::Integer) where T = begin
    print(i)
    Char((s.size_content << 8(i-1)) >> 8(sizeof(T)-1))
end
Base.collect(s::FlyToken) = getindex.(s, 1:lastindex(s))

==(s::FlyToken, b::String) = begin
    String(s)  == b
end
=#
