
"""
common supertype for all token vectors.



"""
abstract type AbstractTokenVector{T <: TinyToken} <: AbstractVector{AbstractToken}
end

"""
Memory-efficient vector of tokens.

On write access, all tokens with code unit count below 8 are stored as
[`DirectToken`](@ref).

Use this type if you expect a large percentage of tokens having less than
8 code units. If performance matters more than memory efficiency, benchmark
against [`BufferTokenVector`](@ref). Though TokenVector induces additional
conditional code to test if a token is of type DirectToken, it can be faster,
due to better memory locality and cache efficiency.
"""
struct TokenVector <: AbstractVector{Token}
    vec :: Vector{HybridToken}
    data :: IOShared
end

"""
token vector for larger tokens.

All token content is stored in a separate buffer, no DirectToken use.
Because memory usage is always higher than [`TokenVector`](@ref), the
only reason to use it is higher performance. Performance highly depends on
your usage pattern - it needs to be benchmarked. Indicators for a better
performance of BufferTokenVector are

## low percentage of short tokens (code unit count below 8)
## Frequent conversion from token to SubString
## Frequent access to ncodeunits(token) without actually accessing code units

"""
struct BufferTokenVector <: AbstractVector{BufferToken}
    vec :: Vector{FlyToken}
    data :: IOShared
end


"""
The most commen token vector.
"""
const TokenVector = PTokenVector{HybridToken}


"""
private auxiliary type for most AbstractToken implementations,
to reduce copy/pase for methods of different token types

Requirement:

Any subtype must have a *tiny::TinyToken* and a *buffer::String*
field.
"""
abstract type TinyBufferToken <: AbstractToken
end


"""
Predefined category semantics.

The category value range is 0..15.

Category 0..7 use Utf8 encoding, enum names are prefixed with "U".
Category 8..15 use ISO-8859-1 encoding (also called Latin1), prefixed with "I"

Some token categories imply that only ASCII characters are used. In
these cases Utf8 and ISO-8859-1 encodings are identical, and the
encoding bit is used to specify two different but similar token
categories. This holds for U_WHITE and I_EOL, and U_NUMBER and I_INTEGER.

"""
@enum TokenCategory ::UInt8 begin
    U_STRING = 0   # default: some Utf8 string, maybe empty
    U_ESCAPED = 1  # delimited token, delimiter removed
    U_CHAR = 2    # delimited token, delimiter removed
    U_COMMENT = 3 # delimited token, delimiter removed
    U_IDENT = 4   # identifier in UTF8
    U_WHITE = 5   # whitespace, might contain codes >127
    U_NUMBER = 6  # decimal and EXP format, might contain codes >127
    U_SYMBOL = 7  #
    I_STRING = 8   # some string, ISO-8851-1
    I_ESCAPED = 9  #
    I_CHAR = 10   #
    I_COMMENT = 11#
    I_IDENT = 12  # identifier
    I_EOL = 13    # end-of-line marker, all codes <128
    I_INTEGER = 14# integer, all codes <128
    I_SYMBOL = 15 #
end


"parametric type to allow for dispatch on preferred encoding"
struct AsLatin{bool}
end

"""
Flyweight string with an associated token category.

This string data type directly stores very short strings with a length
of up to 7 code units. It supports the AbstractString API and can be
used as a memory efficient substitute for String instances, avoiding
any pointer overhead.

Combined with a code unit buffer, it can hold strings of a length below
2**24. The flyweight value encodes an offset and a length, in this case.

TinyToken can be used directly in application code if its length limit
fits the use case. More common is a combination with an additional code unit
buffer. The TinyToken bits pattern is interpreted differently, in this
case, as an offset (32bit) and a length (24bit).

See [`Token`](@ref) and [`MutableToken`](@ref) for an implementation.


# implementation details

Memory layout is 8 bytes, interpreted as an Int64 in host format
(this can have consequences for serializing, if data is exchanged
between systems of differing endianness).

The encoding of the most significant byte is:
```

bit7: tiny flag.
    bit7=0: this is a self-contained TinyToken with
            0..7 code units
    bit7=1: this is a (category,length,offset) tuple,
            only valid in a context which supplies a
            buffer holding the code unit data.

bit6: encoding flag
    bit6=0: encoding is UTF8 (1..4 bytes per character)
    bit6=1: encoding is ISO-8859-1 (1 byte per character,
            UTF codes 1..255)
bit345: category bitfield, values 0..7
bit012: length bitfield, values 0..7.
        !!must be 0 if bit7=1!!
```

## tiny flag clear: 0..7 code units stored directly

Next significant byte is the 1st code unit or 0 (length 0).
... Last significant byte is 7th code unit or 0 (length <7).

This storage format allows for fast string comparison
by lexicographic code units: simply mask off the most
significant byte, reinterpret as Int64 or UInt64 and do
an integer compare.

## tiny flag set: 32 bit offset, 24 bit length
If the tiny flag is set, the four least significant
bytes are interpreted as an UInt32, giving an offset,
and the 4 most significant bytes are interpreted
as an UInt32, giving the length in its lower 24-bit.

The context must suppy a buffer containing code units.
The offset gives the number of code units in the buffer
before the first code unit of this TinyToken.

Context is either an additional parameter in the
functions dealing with a TinyToken, or an additional
field in a larger AbstractToken structure.
"""
struct TinyToken <: AbstractToken
    bits::Int64

    """
    default constructor
    """
    function TinyToken(cat::TokenCategory, s::AbstractString)

        new(bits)
    end

    "override default constructor to convert anything to a string token"
    TinyToken (value::Any) = TinyToken (U_STRING,string(value))
    function TinyToken(::AsLatin{true},s::AbstractString)
    end
    function TinyToken(t::AbstractToken)
    end
end

const maxTinySize = 7 # maximal size of a direct string in TinyToken


"""
Immutable token, characteristics similar to SubString
"""
struct Token <: TinyBufferToken
    tiny :: TinyToken # current value, offset referencing buffer
    buffer :: String # memory with token text data.
    # TODO any negative impact because of pointer field??
end


"""
Mutable token, able to share its buffer with other tokens
and ['SubString']@ref s
"""
mutable struct MutableToken <: TinyBufferToken
    tiny :: TinyToken # current value, offset referencing buffer
    buffer :: String # PRIVATE!! memory with token text data.
    shared :: UInt32 #last index in buffer shared with other tokens
end





"""
offset is 0 for directly stored CUs, or the offset into
the referenced buffer (not given as parameter, because not needed here)
"""
function offset end


function offset(t::TinyToken)
    mask = (t.bits>>63) & (2^32-1) # 0 for direct CU, 0xffffffff else
    convert(UInt32,t.bits & mask)
end
offset(t::TinyBufferToken) = offset(t.tiny)

function Base.ncodeunits(t::TinyToken)
    # tricky code without jumps:
    # if s.bits is positive, we have ncodeunits in lowest 3 bits
    # this extracts the right part in final convert.
    # otherwise, by convention these bits are 0, and we have the length
    # in 24 bits in the bytes following the most significant one.
    # we construct this mask by arithmetic shift, duplicating the
    # most significant bit which is 1 in this case, otherwise 0
    masklen = (t.bits >> 31) & ((2^24-1)<<32) ## length bitfield mask
    convert(Int, ((t.bits &masklen)>>>32) | ((t.bits & (7<<56))>>>56))
end

Base.ncodeunits(t::TinyBufferToken) = ncodeunits(t.tiny)

# helper functions
boundserr(t,i) = throw(BoundsError(t,i))

"""
true, if code unit is UInt8 and 1 code unit == 1 character
"""
function islatin end

islatin(t::AbstractToken) = (category(t)&8) > 0
islatin(t::TinyToken) = t.bits&(1<<62) > 0
islatin(t::TinyBufferToken) = islatin(t.tiny)
# following method is quite expensive ...
islatin(s::AbstractString) = (codeunit(s)==UInt8) && (ncodeunits(s)=length(s))


"""
returns the category of a token
"""
function category end
category(t::TinyToken) = TokenCategory((t.bits>>59)&15)
category(t::TinyBufferToken) = category(t.tiny)

Base.thisind(t::AbstractToken, i::Integer) = thisind(t,convert(Int,i))
function Base.thisind(t::AbstractToken, i::Int)
    if i<=0
        @boundscheck i==0 || boundserr(t,i)
        return 0
    else
        lenp1 = ncodeunits(t)+1
        if i>=lenp1
            @boundscheck i==lenp1 || boundserr(t,i)
            return i
        end
        # now we have a normal case: i is valid index, calculate
        islatin(t) && return i
        # poor boy ... we must really find the index. copy/paste from strings.jl
        @inbounds b = codeunit(s, i)
        (b & 0xc0 == 0x80) & (i-1 > 0) || return i
        @inbounds b = codeunit(s, i-1)
        between(b, 0b11000000, 0b11110111) && return i-1
        (b & 0xc0 == 0x80) & (i-2 > 0) || return i
        @inbounds b = codeunit(s, i-2)
        between(b, 0b11100000, 0b11110111) && return i-2
        (b & 0xc0 == 0x80) & (i-3 > 0) || return i
        @inbounds b = codeunit(s, i-3)
        between(b, 0b11110000, 0b11110111) && return i-3
        return i
    end
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
