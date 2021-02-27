
"""
Central token type: a flyweight token plus its buffer
"""
struct Token{F<:FlyToken} <: AbstractToken
    fly :: F # category, size, offset, possibly content
    buffer :: String # memory with token text data or EMPTYSTRING
    Base.@propagate_inbounds function Token{F}(fly::F, buffer::String) where F<:FlyToken
        @boundscheck isdirect(fly) || checklimit(offset(fly)+usize(fly),usize(buffer))
        new(fly,buffer)
    end
end


## GenericToken constructors

Base.@propagate_inbounds function Token(fly::F, buffer::String) where F<:FlyToken
    Token{F}(fly,buffer)
end



"""
Recommended general-purpose Token implementation.

Immutable token, contents either directly stored or buffer based.
Use this type if you expect many token sizes below 8, and want to reduce
memory consumptionand better data locality. It comes at the price of an
additional comparison for most token operations (casting to BToken or DirectFly). 

# implementation hint:

Constructors using Strings as content source will encode contents with
up to 7 bytes directly. Only construction from BToken will use buffered content,
even if byte count is below 8. To guarantee content is always buffer based
(which is not the intention behind the Token type), construct using a
BToken. 
"""
const HToken = Token{HybridFly}


# Generation of the string literal macros for tokens

# try export in eval             #:(:export, Symbol('@',$($(ttype)),$($(ccat)),"_str"))

for ttype in ['D','H','B']
    for cat in 0:15
        ccat = Char(cat<=9 ? '0'+cat : 'A'+cat-10)
        eval(quote
            macro $(Symbol(ttype,ccat,"_str"))(txt)
                :($(Symbol($(ttype),"Token"))(Nibble($($cat)),$txt))
            end
        end)
        s = Symbol('@',ttype,ccat,"_str")
        @eval export $s
    end
end


function Base.show(io::IO,t::Token{T}) where T 
    print(io, T <: HybridFly ? 'H' : 'B',Char(category(t)))
    Base.print_quoted(io, t)
end


"""
Immutable token, always buffer based.

Use this type if you expect mostly a token size above 7
"""
const BToken = Token{BufferFly}

HToken(t::BufferFly,s::String) = HToken(hf(t),s)
HToken(t::DirectFly) = @inbounds HToken(hf(t),EMPTYSTRING)
HToken(t::BToken) = @inbounds HToken(t.fly,t.buffer)
HToken(t::HToken) = t

BToken(t::BToken) = t
BToken(t::HToken) = isdirect(t) ? BToken(df(t.fly)) : BToken(bf(t.fly),t.buffer)

Token{F}(cat::Nibble, s::String) where F<:FlyToken = @inbounds Token{F}(cat,0%UInt32,usize(s),s)
Token{F}(cat::Nibble) where F<:FlyToken = @inbounds Token{F}(cat,EMPTYSTRING)
Token{F}(t::AbstractToken) where F<:FlyToken = Token{F}(category(t),0%UInt32,usize(t),substring(t))


Base.@propagate_inbounds function Token{BufferFly}(cat::Nibble,offset::UInt32, size::UInt64, s::String)
    size == 0 && return BToken(cat)
    BToken(BufferFly(cat,size)+offset,s)
end


Base.@propagate_inbounds function HToken(cat::Nibble,offset::UInt32, size::UInt64, s::String)
    size <= MAX_DIRECT_SIZE && return HToken(DirectFly(cat,offset,size,s))
    HToken(BufferFly(cat,size)+offset,s)
end


Token{T}(cat::Nibble,s::SubString{String}) where T = 
  Token{T}(cat,s.offset,usize(s),s.string)


Token{T}(cat::Nibble,s::AbstractString)  where T = 
  Token{T}(cat,string(s))


Base.@propagate_inbounds function BToken(offset::UInt32, size::UInt64,t::Union{Token,DirectFly}) 
    @boundscheck checklimit(offset+size,usize(t))
    @inbounds isdirect(t) ? BToken(category(t),offset,size,string(t)) : BToken(category(t),offset+offset(t),size,t.buffer)
end


Base.@propagate_inbounds HToken(offset::UInt32, size::UInt64,t::Union{Token,DirectFly}) =
    isdirect(t) ? HToken(DirectFly(offset,size,t)) : HToken(BToken(offset,size,t))


# conversions of standard types
Token{F}(v::Integer) where F<:FlyToken = Token{F}(T_INT,string(v))
Token{F}(v::Real) where F<:FlyToken =    Token{F}(T_REAL,string(v))
Token{F}(v::Char) where F<:FlyToken =    Token{F}(T_CHAR,v)
Token{F}(v::Bool) where F<:FlyToken=    Token{F}(T_KEY,string(v))
Token{F}(v::AbstractString) where F<:FlyToken= Token{F}(T_TEXT,v)
# was soll das? Token{F}(offset::UInt32, size::UInt64,s::String) = where F<:FlyToken Token{F}(T_TEXT,s)


DirectFly(t::Token{T}) where T = isdirect(t) ? df(t.fly) : DirectFly(category(t),offset(t),usize(t),t.buffer)



## special values for missing and nothing

Base.ismissing(t::Token) = df(t.fly & ~NOTDIRECT_BIT) == DIRECT_MISSING
Base.isnothing(t::Token) = df(t.fly & ~NOTDIRECT_BIT) == DIRECT_NOTHING
HToken(::Nothing) = HToken(DIRECT_NOTHING)
BToken(::Nothing) = BToken(BufferFly(nothing))
HToken(::Missing) = HToken(DIRECT_MISSING)
BToken(::Missing) = BToken(BufferFly(missing))

## Token API methods ##

offset(t::Token) = offset(t.fly)

category(t::Token) = category(t.fly)

isdirect(t::Token) = isdirect(t.fly)

usize(t::Token) = usize(t.fly)


## ByteString API

"""
ByteString is a family of types which support the following API:

 * pointer(t) -> Ptr{UInt8} 

 * usize(t) -> number of bytes as UInt64

 * byte(t,ofs::UInt32) -> UInt8 value at offset ofs
"""
const ByteString = Union{String,SubString{String},BToken,Vector{UInt8},Vector{Int8}}


usize(s::Union{Vector{UInt8},Vector{Int8}}) = length(s)

"codeunit with offset (aka 0-based index)"
Base.@propagate_inbounds function byte(s::ByteString, ofs::UInt32)
    @boundscheck checkbyteofs(ofs,usize(s))
    b = GC.@preserve s unsafe_load(pointer(s)+ofs)
    return b
end

Base.@propagate_inbounds function byte(s::HToken, ofs::UInt32)
    if isdirect(s) 
        return byte(s.fly,ofs)
    end
    @boundscheck checkbyteofs(ofs,usize(s))
    @inbounds byte(s.buffer,ofs+offset(s))
end


## Base methods for tokens ##

Base.pointer(t::BToken) = pointer(t.buffer)+offset(t)

Base.cmp(a::HToken, b::DirectFly) = isdirect(a.fly) ? cmp(a.fly,b) : cmp_codeunits(a,b)

Base.cmp(a::DirectFly, b::HToken) = isdirect(b.fly) ? cmp(a,b.fly) : cmp_codeunits(a,b)

Base.cmp(a::HToken, b::HToken) = reinterpret(Int64,u64(a.fly)|u64(b.fly))>=0 ? cmp(a.fly,b.fly) : cmp_codeunits(a,b)


## substring implementations

Base.@propagate_inbounds function substring(offset::UInt32, size::UInt64, t::Token{F}) where F
    isdirect(t) && return substring(offset,size,df(t.fly))
    @boundscheck checklimit(offset+size,usize(t)) # necessary to ensure substring is in bounds of s, not only of s.string
    @inbounds return substring(offset+t.offset,size,t.buffer)
end


## converts

Base.convert(::Type{Token{F}}, s::AbstractString) where F <:FlyToken = Token{F}(T_TEXT,s)

Base.convert(::Type{Token{F}}, t::AbstractToken) where F <:FlyToken = Token{F}(t)

Base.convert(::Type{SubString{String}}, t::BToken) = @inbounds SubString(offset(t),usize(t),t.buffer)

Base.convert(::Type{SubString{String}}, t::HToken) = convert(SubString{String},BToken(t)) 

Base.convert(::Type{SubString{String}}, t::DirectFly) = convert(SubString{String},string(t)) 

Base.convert(::Type{String}, t::AbstractToken) = string(t) 


function Base.codeunit(t::Token, i::Int)
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


function Base.hash(t::HToken, h::UInt)
    isdirect(t) && return hash(df(t.fly),h)
    hash(BToken(t),h)
end


#=
function Base.iterate(t::Token, i::Integer=firstindex(t))
    i > ncodeunits(t) && return nothing
    @boundscheck checklimiti <= 0 && @boundscheck checkbounds(t, i) # ?? wozu??
    o = offset(t)
    if isdirect(t)
        y = iterate(df(t.fly),i)
    else
        y = iterate(t.buffer, o + i)
    end
    c, i = y
    return c, i - o
end
=#

function Base.getindex(t::Token, i::Integer)
    @boundscheck checkbounds(t, i)
    @inbounds if isdirect(t)
        getindex(df(t.fly),i)
    else
        getindex(t.buffer, offset(bf(t.fly)) + i)
    end
end



function Base.read(io::IO, ::Type{HToken})
    cat_size = read(io,Packed31)
    size = bits4_30(cat_size)
    if size <= MAX_DIRECT_SIZE
        t = read(io,cat_size,DirectFly)
        HToken(t)
    else
        @inbounds HToken(Nibble(bits0_3(cat_size)), read(io,size,String))
    end
end


function Base.read(io::IO, ::Type{BToken})
    cat_size = read(io,Packed31)
    size = bits4_30(cat_size)
    @inbounds BToken(BufferFly(cat_size), read(io,size,String))
end


# serialize Token
function Base.write(io::IO, t::Token)
    tt = t.fly
    if isdirect(tt) # optimized away for BufferFly
        write(io,df(tt))
    else
        size = usize(bf(tt))
        p = Packed31(category(tt)%UInt8, size%UInt32)
        write(io,p)
        twrite(io,offset(bf(tt)),size,t.buffer)
    end
end

