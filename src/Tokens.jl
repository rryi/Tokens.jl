module Tokens

"""
code unit for tokens
"""
const CU = UInt8 # currently, only support bytes as code units

# abstract type AbstractToken{C} where {C<:Integer} <: AbstractString

"""
A categorized string, supertype of all token types in this module.

In addition to the AbstractString API, an AbstractToken has a
[`TokenCategory`](@ref) and supports dynamic encoding in Utf8
or ISO-8859-1 (also called Latin1).

This module supplies very memory efficient implementations
for very short tokens with [`TinyToken`](@ref),
field String wFlyweight string with an associated token category.

This string data type directly stores very short strings with a length
of up to 7 code units. It supports the AbstractString API and can be
used as a memory efficient substitute for String instances, avoiding
any pointer overhead.

In addition to its AbstractString API,

Together wit a code unit buffer, it can hold strings of a length up to
2**24.
It can also hold longer strings, but then, a buffer must be supplied
which holds all code units. The flyweight value encodes an offset and
a length, in this case.
"""
abstract type AbstractToken <: AbstractString
end



"""
Predefined category semantics.

The category value range is 0..15.

Category 0..7 use Utf8 encoding, enum names are prefixed with "U".
Category 8..15 use ISO-8859-1 encoding (also called Latin1), prefixed with "I"

"""
@enum TokenCategory ::UInt8 begin
    #"Some comment"
    TOKEN_BLOCK = 0
    LCAT_STRING = 1
    LCAT_CHARS = 2
    LCAT_COMMENT = 3
    LCAT_IDENT = 4
    LCAT_WHITE
    LCAT_NUMBER = 6
    LCAT_SYMBOL = 7
end


"""
Flyweight string with an associated token category.

This string data type directly stores very short strings with a length
of up to 7 code units. It supports the AbstractString API and can be
used as a memory efficient substitute for String instances, avoiding
any pointer overhead.

In addition to its AbstractString API,

Together wit a code unit buffer, it can hold strings of a length up to
2**24.
It can also hold longer strings, but then, a buffer must be supplied
which holds all code units. The flyweight value encodes an offset and
a length, in this case.



# inplementation details

Memory layout is 8 bytes stored as an Int64.
Most significant byte stores type, category, encoding and length.
Next significant byte is the 1st code unit or 0 (length 0).
...
Last significant byte is 7th  code unit or 0 (length <7).

The encoding of the most significant byte is:
bit7: type flag.
bit7=0: this is a valid TinyToken with 0..7 code units
bit7=1: this is a (category,length,offset) tuple,
        only valid with reference to a buffer holding
        the CU data starting at offset

bit6: encoding flag
bit6=0: encoding is Latin1 (1 byte per character, UTF codes 1..255)
bit6=1: encoding is UTF8 (1..4 bytes per character)

bit345: category bitfield, values 0..7
bit012: length bitfield, values 0..7.
        !!must be 0 if bit7=1!!

offsetis a
interpreted as an Int64 in little endian format, the
following bit fields are encoded:

byte8, bit8: 0=direct string, 1=buffered string
this is the sign bit of the Int64!
It determines interpretation of the rest of TinyToken:

## direct string: >=0 (byte8, bit8 ==0)
byte8, bit1..3: length 0..7
byte1 is the last code unit
byte2 the 2nd last code unit
...
byte(8-s) the 1st code unit.
if s<7, byte(s+1..7) are 0.

This format allows to compare

## buffered string: <0 (byte8, bit8 ==1)

byte7, Bit0..2: length 0..7

if direct string, the rest is interpreted as follows:
byte0..byte6
byte0

"""
primitive type TinyToken <: AbstractToken 64 end

const maxTinySize = 7 # maximal size of a direct string in TinyToken

mutable struct Token
    tiny :: TinyToken # current value, offset referencing buffer
    buffer :: Vector{CU} # memory with token text data.
    # TODO use C String ptr and limit index
    offset :: UInt32 # value to add to 1 to get index of 1st CU in buffer
    shared :: UInt32 #last index in buffer shared with other tokens
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
