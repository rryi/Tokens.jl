## complete (buffered) token implementations


"""
private auxiliary type for the AbstractToken implementations here,
to reduce copy/pase for methods of different token types.

Requirement:

Any subtype must have a *tiny::TinyToken* and a *buffer::String*
field.
"""
abstract type TinyBufferToken <: AbstractToken
end


"""
Recommended general-purpose Token implementation.

Immutable token, either direct enoded or buffer based.
"""
struct Token <: TinyBufferToken
    tiny :: HybridToken # category, size, offset
    buffer :: String # memory with token text data or EMPTYSTRING
    Base.@propagate_inbounds function Token(tiny::HybridToken, buffer::String)
        @boundscheck isdirect(tiny) || checkbounds(buffer,offset(tiny)+ncodeunits(tiny))
        new(tiny,buffer)
    end
end


"""
Immutable token, always buffer based

Use this type if you expect most token sizes to be above 7
"""
struct BufferToken <: TinyBufferToken
    tiny :: FlyToken # category, size, offset
    buffer :: String # memory with token text data.
    BufferToken(t::DirectToken) =
        new(FlyToken(category(t),ncodeunits(t)),string(t))
    function BufferToken(t::Token)
        if isdirect(t)
            BufferToken(dt(t.tiny))
        else
            new(ft(t.tiny),t.buffer)
        end
    end
    function BufferToken(cat::TCategory, s::String)
        new(FlyToken(cat,ncodeunits(s)%UInt64),s)
    end
    Base.@propagate_inbounds function BufferToken(
        cat::TCategory,offset::UInt32, size::UInt64, s::String)
        @boundscheck check_ofs_size(offset,size,s)
        new(FlyToken(cat,offset,size),s)
    end
    function BufferToken(cat::TCategory,s::SubString{String})
        new(FlyToken(cat,UInt32(s.offset),s.ncodeunits%UInt64),s.string)
    end
end

BufferToken(cat::TCategory,s::AbstractString) = BufferToken(cat,string(s))
BufferToken(t::BufferToken) = t

Token(cat::TCategory) = Token(cat,EMPTYSTRING)
BufferToken(cat::TCategory) = BufferToken(cat,EMPTYSTRING)
Token(cat::TCategory,s::AbstractString) = Token(cat,string(s))
Token(t::DirectToken) = @inbounds Token(ht(t),EMPTYSTRING)
Token(t::BufferToken) = @inbounds Token(ht(t.tiny),t.buffer)
Token(t::Token) = t
DirectToken(t::Token) = isdirect(t) ? dt(t.tiny) : DirectToken(category(t),t)


Base.@propagate_inbounds function Token(
    cat::TCategory,offset::UInt32, size::UInt64, s::String, direct::Bool=false)
    if direct && (size<=MAX_DIRECT_SIZE)
        new(ht(DirectToken(cat,offset,size,s)),EMPTYSTRING)
    else
        @boundscheck check_ofs_size(offset,size,ncodeunits(s))
        new(ht(FlyToken(cat,offset,size)),s)
    end
end

Base.@propagate_inbounds function Token(cat::TCategory, s::Utf8String, direct::Bool=false)
    size = s.ncodeunits
    if direct && (size <= MAX_DIRECT_SIZE)
        Token(ht(DirectToken(cat,s)),EMPTYSTRING)
    else
        Token(BufferToken(cat,s))
#        @boundscheck checksize(size,MAX_TOKEN_SIZE)
#        @inbounds Token(ht(FlyToken(cat,s.offset,size)),s.string)
    end
end




## Token API methods ##

offset(t::TinyBufferToken) = offset(t.tiny)

category(t::TinyBufferToken) = category(t.tiny)

isdirect(t::TinyBufferToken) = isdirect(t.tiny)


usize(t::TinyBufferToken) = usize(t.tiny)


## Base methods for tokens ##

Base.cmp(a::Token, b::DirectToken) = isdirect(a.tiny) ? cmp(a.tiny,b) : cmp_codeunits(a,b)

Base.cmp(a::DirectToken, b::Token) = isdirect(b.tiny) ? cmp(a,b.tiny) : cmp_codeunits(a,b)

Base.cmp(a::Token, b::Token) = reinterpret(Int64,u64(a.tiny)|u64(b.tiny))>=0 ? cmp(a.tiny,b.tiny) : cmp_codeunits(a,b)

Base.convert(::Type{BufferToken}, s::AbstractString) = BufferToken(T_TEXT,s)

Base.convert(::Type{Token}, s::AbstractString) = Token(T_TEXT,s)

Base.convert(::Type{BufferToken}, t::AbstractToken) = BufferToken(t)

Base.convert(::Type{Token}, t::AbstractToken) = Token(t)

# any use case?? better work with tokens directly, do not convert to (Sub)String
Base.convert(::Type{SubString{String}}, t::BufferToken) =
    @inbounds SubString(t.buffer,offset(t)+1,thisind(offset(t)+ncodeunits(t)))


function Base.codeunit(t::TinyBufferToken, i::Int)
    if isdirect(t.tiny) # optimized away for BufferToken because isdirect is constantly false
        codeunit(dt(t.tiny),i)
    else
        @boundscheck checkbounds(t, i)
        @inbounds return codeunit(t.buffer, offset(ft(t.tiny)) + i)
    end
end



# any advantage over default impl?? TODO -> look at generated default code
# try & test with default impl!
#=
function Base.iterate(t::TinyBufferToken, i::Integer=firstindex(s))
    i == ncodeunits(t)+1 && return nothing
    @boundscheck checkbounds(t, i)
    y = iterate(t.buffer, t.offset + i)
    y === nothing && return nothing
    c, i = y
    return c, i - t.offset
end

function Base.getindex(t::BufferToken, i::Integer)
    @boundscheck checkbounds(t, i)
    @inbounds return getindex(t.buffer, offset(t) + i)
end

function Base.getindex(t::Token, i::Integer)
    @boundscheck checkbounds(t, i)
    @inbounds if isdirect(t)
        getindex(dt(t.tiny),i)
    else
        getindex(t.buffer, offset(ft(t.tiny)) + i)
    end
end
=#


function Base.read(io::IO, ::Type{Token})
    cat,size = _readtokenheader(io)
    if size <= MAX_DIRECT_SIZE
        t = ht(_readdirecttoken(io,DirectToken(cat,size)))
        @inbounds Token(t,EMPTYSTRING)
    else
        @inbounds Token(ht(FlyToken(cat,size)), read(io,size,String))
    end
end


function Base.read(io::IO, ::Type{BufferToken})
    cat,size = _readtokenheader(io)
    @inbounds BufferToken( FlyToken(cat,size), read(io,size,String))
end



# serialize Token and BufferToken
function Base.write(io::IO, t::TinyBufferToken)
    tt = t.tiny
    if isdirect(tt) # optimized away for BufferToken (always false)
        write(io,dt(tt))
    else
        f = ft(tt)
        _writetokenheader(io,f)
        write(io,offset(f),usize(f),t.buffer)
    end
end



#=

String(s::TinyToken) = String(reinterpret(UInt8, [s.size_content|>ntoh])[1:sizeof(s)])

Base.lastindex(s::TinyToken) = Int(s.size_content & 0xf)
Base.iterate(s::TinyToken, i::Integer) = iterate(String(s), i)
Base.iterate(s::TinyToken) = iterate(String(s))
Base.sizeof(s::TinyToken) = Int(s.size_content & 0xf)
Base.print(s::TinyToken) = print(String(s))
Base.display(s::TinyToken) = display(String(s))
Base.convert(::TinyToken{T}, s::String) where T = TinyToken{T}(s)
Base.convert(::String, ss::TinyToken) = String(a) #reduce(*, ss)
Base.firstindex(::TinyToken) = 1
Base.ncodeunits(s::TinyToken) = ncodeunits(String(s))
Base.codeunit(s::TinyToken, i) = codeunits(String(s), i)
Base.isvalid(s::TinyToken, i::Integer) = isvalid(String(s), i)

Base.getindex(s::TinyToken{T}, i::Integer) where T = begin
    print(i)
    Char((s.size_content << 8(i-1)) >> 8(sizeof(T)-1))
end
Base.collect(s::TinyToken) = getindex.(s, 1:lastindex(s))

==(s::TinyToken, b::String) = begin
    String(s)  == b
end
=#
