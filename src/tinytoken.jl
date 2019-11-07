# TinyToken type and associated methods

const maxTinySize = 7 # maximal size of a direct string in TinyToken

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
case, as an offset (32bit) and a length (27bit).

See [`Token`](@ref) and [`MutableToken`](@ref) for an implementation.

# warning on @inbounds usage

Methods that have a TinyToken parameter, need access to its code units,
but have no associated code unit buffer as a parameter, will check if
code units are stored as part of the supplied token. If not, they will
 throw a BoundsError.

This check is annotated with @boundscheck, so it is probably skipped
if the method is called in an @inbounds block.

Code annotated with @inbounds assures not only that all used indices are in
bounds. It additionally assures that all code units are directly stored
in the TinyToken instance for all method calls with a TinyToken parameter
which is not accomplished by a code unit buffer parameter.

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
bit3456: category bitfield, values 0..15
bit012: length bitfield, values 0..7.
        !!contains lowest length bits if bit7=1!!
```

## tiny flag clear: 0..7 code units stored directly

Next significant byte is the 1st code unit or 0 (length 0).
... Last significant byte is 7th code unit or 0 (length <7).

This storage format allows for fast string comparison
by lexicographic code units: simply mask off the most
significant byte, reinterpret as Int64 or UInt64 and do
an integer compare.

## tiny flag set: 32 bit offset, 27 bit length
If the tiny flag is set, the four least significant
bytes are interpreted as an UInt32, giving an offset,
and the 4 most significant bytes are interpreted
as an UInt32, giving the length in its lower 24 bits
plus 3 bits in the most significant byte, for a total
of 27 bits for the length encoding.

The context must supply a buffer containing code units.
The offset gives the number of code units in the buffer
before the first code unit of this TinyToken.

Context is either an additional parameter in the
functions dealing with a TinyToken, or an additional
field in a larger AbstractToken structure.

If tiny flag is set and a method does not know which
buffer is referenced, it must raise an exception.
"""
struct TinyToken <: AbstractToken
    bits::Int64

    """
    default constructor
    """
    function TinyToken(cat::UInt8, s::String, first::Int, size::Int)
        @boundscheck checkbounds 0 < first && first+size <= ncodeunits(s) ||
        @boundscheck size < 8 || throw BoundsError("Size too large for TinyToken string constructor",size)
        new(bits)
    end


    function TinyToken(c::Char)
        bits ::UInt64 =
          (Base.codelen(c)) << 56) |
          reinterpret(UInt32, c) << 24
       new(bits)
   end

    "override default constructor to convert anything to a string token"
    TinyToken (value::Any) = TinyToken (U_STRING,string(value))
    function TinyToken(::AsLatin{true},s::AbstractString)
    end
    function TinyToken(t::AbstractToken)
    end
end


function offset(t::TinyToken)
    mask = (t.bits>>63) & (2^32-1) # 0 for direct CU, 0xffffffff else
    convert(UInt32,t.bits & mask)
end

function Base.ncodeunits(t::TinyToken)
    # tricky code without jumps:
    # if s.bits is positive, we have ncodeunits in lowest 3 bits
    # this extracts the right part in final convert.
    # otherwise, by convention these bits are 0, and we have the length
    # in 24 bits in the bytes following the most significant one.
    # we construct this mask by arithmetic shift, duplicating the
    # most significant bit which is 1 in this case, otherwise 0
    masklen = (t.bits >> 31) & ((2^24-1)<<32) ## length bitfield mask
    ((t.bits &masklen)>>>29) | ((t.bits & (7<<56))>>>56)
end


function Base.cmp(a::TinyToken, b::TinyToken)
    @checkbounds checktiny(a)
    @checkbounds checktiny(b)
    # both tiny: compare all code units in one step
    (a.bits& 2^56-1) < (b.bits& 2^56-1) && return -1
    (a.bits& 2^56-1) > (b.bits& 2^56-1) && return 1
    0
end



category(t::TinyToken) = UInt8((t.bits>>59)&15)




"throw an error if token references some buffer"
function checktiny(t::TinyToken)
    t.bits>=0 || throw(ErrorException("token references unknown buffer: &t"))
end


Base.show(io::IO, t::TinyToken)
    print(io,'^',category(t))
    if t.bits>=0
        Base.print_quoted(io, t)
    else
        print(io,"$[ofs=",offset(t),",n=",ncodeunits(t),']')
    end
end


"bitmask to check if any inline code unit is not ascii"
const nonasciibits :: Int64 = reinterpret(Int64,0x10000000100000001000000010000000100000001000000010000000)

function Base.isascii(t::TinyToken)
    @boundscheck checktiny(t)
    t.bits & nonasciibits == 0
end

@propagate_inbounds function Base.codeunit(t::TinyToken, i::Integer)
    @boundscheck 0 < i <= ncodeunits(t) || boundserr(t,i)

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
