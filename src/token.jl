
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

#= variable type generation - may cause type instability
for cat in instances(TCategory)
    eval(quote
        macro $(Symbol(Symbol(cat),"_str"))(txt)
            if ncodeunits(txt)>7
                :(BToken($($(cat)),$txt))
            else
                :(DirectFly($($(cat)),$txt))
            end
        end
        export $(Symbol(cat))
        export @$(Symbol(Symbol(cat),"_str"))
    end)
end
=#


for cat in 0:15
    ncat = Nibble(cat)
    ccat = Char(cat<=9 ? '0'+cat : 'A'+cat-10)
    eval(quote
        macro $(Symbol('D',ccat,"_str"))(txt)
            :(DirectFly(:(Nibble($cat),$txt)))
        end
        export @$(Symbol('D',ccat,"_str"))
    end)
end

# TODO ditto HToken, BToken

function Base.show(io::IO,t::Token{T}) where T 
    print(io, T <: HybridFly ? 'H' : 'B')
    show(category(t))
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

Token{T}(cat::TCategory, s::String) where T = @inbounds Token{T}(cat,0,usize(s),s)
Token{T}(cat::TCategory) where T = @inbounds Token{T}(cat,EMPTYSTRING)
Token{T}(t::AbstractToken) where T = Token{T}(category(t),0,usize(t),substring(t))


Base.@propagate_inbounds function BToken(cat::TCategory,offset::UInt32, size::UInt64, s::String)
    size == 0 && return BToken(cat)
    BToken(BufferFly(cat,size)+offset,s)
end


Base.@propagate_inbounds function HToken(cat::TCategory,offset::UInt32, size::UInt64, s::String)
    size <= MAX_DIRECT_SIZE && return HToken(DirectFly(cat,offset,size,s))
    HToken(BufferFly(cat,size)+offset,s)
end


Token{T}(cat::TCategory,s::SubString{String}) where T = 
  Token{T}(cat,s.offset,ncodeunits%UInt64,s.string)


Token{T}(cat::TCategory,s::AbstractString)  where T= 
  Token{T}(cat,s.offset,ncodeunits%UInt64,string(s))


Base.@propagate_inbounds function BToken(offset::UInt32, size::UInt64,t::Union{Token,DirectFly}) 
    @boundscheck checklimit(offset+size,usize(t))
    @inbounds isdirect(t) ? BToken(category(t),offset,size,string(t)) : BToken(category(t),offset+offset(t),size,t.buffer)
end


Base.@propagate_inbounds HToken(offset::UInt32, size::UInt64,t::Union{Token,DirectFly}) =
    isdirect(t) ? HToken(DirectFly(offset,size,t)) : HToken(BToken(offset,size,t))


# conversions of standard types
Token{T}(v::Integer) where T = Token{F}(T_INT,string(v))
Token{T}(v::Real) where T =    Token{F}(T_REAL,string(v))
Token{T}(v::Char) where T =    Token{F}(T_CHAR,v)
Token{T}(v::Bool) where T =    Token{F}(T_KEY,string(v))
Token{T}(v::AbstractString) where T = Token{F}(T_TEXT,v)
Token{T}(offset::UInt32, size::UInt64,s::String) where T = Token{F}(T_TEXT,v)


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

function Base.getindex(t::HToken, i::Integer)
    @boundscheck checkbounds(t, i)
    @inbounds if isdirect(t)
        getindex(df(t.fly),i)
    else
        getindex(t.buffer, offset(bf(t.fly)) + i)
    end
end
=#


function Base.read(io::IO, ::Type{HToken})
    cat_size = read(io,Packed31)
    size = bits4_30(cat_size)
    if size <= MAX_DIRECT_SIZE
        t = read(io,cat_size,DirectFly)
        HToken(t)
    else
        @inbounds HToken(TCategory(bits0_3(cat_size)), read(io,size,String))
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

