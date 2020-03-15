## Token, MutableToken implementations


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

Immutable token, either direct enoded or buffer based
"""
struct Token <: TinyBufferToken
    tiny :: HybridToken # category, size, offset
    buffer :: String # memory with token text data or EMPTYSTRING
end

Token(t::BufferToken) = Token(ht(t.tiny),t.buffer)

Token(t::DirectToken) = Token (ht(t),EMPTYSTRING)


@propagate_inbounds function Token(cat:TCategory, s::Utf8String, direct::Bool=false)
    size = s.ncodeunits
    if direct && (size <= MAX_DIRECT_SIZE)
        Token(ht(DirectToken(cat,s)),EMPTYSTRING)
    else
        Token(BufferToken(cat,s))
#        @boundscheck checksize(size,MAX_TOKEN_SIZE)
#        @inbounds Token(ht(FlyToken(cat,s.offset,size)),s.string)
    end
end

"""
Immutable token, always buffer based

Use this type if you expect most token sizes to be above 7
"""
struct BufferToken <: TinyBufferToken
    tiny :: FlyToken # category, size, offset
    buffer :: String # memory with token text data.
end

BufferToken(t::DirectToken) = BufferToken(FlyToken(category(t),sizeof(t)),string(t))

function BufferToken(t::Token)
    if isdirect(t)
        BufferToken(dt(t.tiny))
    else
        BufferToken(ft(t.tiny),t.buffer)
    end
end

function BufferToken(cat:TCategory, s:SubString{String})
    BufferToken(FlyToken(cat,s.offset,s.ncodeunits),s.string)
end

function BufferToken(cat:TCategory, s:String)
    BufferToken(FlyToken(cat,ncodeunits(s)),s)
end




#= maybe ... currently, not really helpful
@bitflag BufferFlags ::UInt32 begin
    TRYDIRECT # return kokens of size<8 as DirectToken

    NONE = 0
end
=#



# Base._string_n
# allocates and returns a String witn n code units

#= alloc und Schreiben in String
ss = _string_n(n)
p = pointer(ss)
for k = 1:n
    unsafe_store!(p, codeunit(s, i + k - 1), k)
endmethods(length)

# out == IObuffer transferiert String ptr
String(take!(out))

=#


#########################################################
################ Token API methods ######################
#########################################################

offset(t::TinyBufferToken) = offset(t.tiny)

category(t::TinyBufferToken) = category(t.tiny)

isDirect(t::TinyBufferToken) = isDirect(t.tiny)


#########################################################
############## Base methods for tokens ##################
#########################################################

Base.sizeof(t::TinyBufferToken) = sizeof(t.tiny)


Base.cmp(a::Token, b::DirectToken) = isdirect(a.tiny) ? cmp(a.tiny,b) : cmp_codeunits(a,b)

Base.cmp(a::DirectToken, b::Token) = isdirect(b.tiny) ? cmp(a,b.tiny) : cmp_codeunits(a,b)

Base.cmp(a::Token, b::Token) = reinterpret(Int64,u64(a.tiny)|u64(b.tiny))>=0 ? cmp(a.tiny,b.tiny) : cmp_codeunits(a,b)

Base.convert(::Type{BufferToken}, s::SubString{String}) = BufferToken(T_TEXT,s)

Base.convert(::Type{Token}, s::SubString{String}) = Token(T_TEXT,s)


function Base.codeunit(t::TinyBufferToken, i::Integer)
    if isdirect(t.tiny)
        codeunit(dt(t.tiny),i)
    else
        @boundscheck checkbounds(t, i)
        @inbounds return codeunit(s.string, s.offset + i)
    end
end

function Base.iterate(s::TinyBufferToken, i::Integer=firstindex(s))
    i == ncodeunits(s)+1 && return nothing
    @boundscheck checkbounds(s, i)
    y = iterate(s.string, s.offset + i)
    y === nothing && return nothing
    c, i = y
    return c, i - s.offset
end

function getindex(s::SubString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds return getindex(s.string, s.offset + i)
end


function Base.SubString(t::TinyBufferToken,i::Int, j::Int)
    if isDirect(t)
    t.tiny<0 ? SubString(t.)
     = SubString()
end

#
function Base.SubString(t::MutableToken,i::Int, j::Int)
    if
    t.tiny<0 ? SubString(t.)
     = SubString()
end


#=
function TinyToken(s::String, category::UInt8=0)
    sz = sizeof(s)
    # try to encode in 7 bytes. tinytoken
    if sz < sizeof(::TinyToken)
        str :: Int64 = 0
        utf8Flag :: UInt8 = 0
        i = 0
        while ++i <= sz
            c = codeunit(s,i)
            (str *= 8) += c
            if c>127
            end

        end
        p :: Ptr{UInt8} = pointer(s)
        (str *= 8) += codeunit(s,i)

        str = (T(s |> pointer |> Ptr{TinyToken} |> Base.unsafe_load |> ntoh)
    end
    # check for UTF8 encoded ISO-8859-1 string: sizeof(s)>=8 could be valid in this case
    throw(ErrorException("supplied string size $sz exceeds size limit for TinyToken - use Token, instead"))

    bits_to_wipe = 8(sizeof(T) - sz)
    content = (T(s |> pointer |> Ptr{TinyToken} |> Base.unsafe_load |> ntoh) >> bits_to_wipe) << bits_to_wipe
    TinyToken{T}(content | T(sz))
end

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

end # module
