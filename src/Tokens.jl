module Tokens

const CU = UInt8 # currently, only support bytes as code units

# abstract type AbstractToken{C} where {C<:Integer} <: AbstractString
abstract type AbstractToken <: AbstractString
end

"""
Flyweight string with an associated token category (0..7).

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

const maxTinySize = 7; # maximal size of a direct string in TinyToken

const isUtf8 :: UInt8 = 8;

mutable struct Token {
    tiny :: TinyToken # current value, offset referencing buffer
    buffer :: Vector{CU} # memory with token text data.
    # TODO use C String ptr and limit index
    offset :: UInt32 # value to add to 1 to get index of 1st CU in buffer
    shared :: UInt32 #last index in buffer shared with other tokens
} end


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

end # module
