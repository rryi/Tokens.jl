# TinyToken type and associated methods

"maximal count of code units directly encoded in a TinyToken"
const MAX_TINY_SIZE = 7

"bitmask to check if any inline code unit is not ascii"
const NOTASCII_BITS :: UInt64 = 0x10000000100000001000000010000000100000001000000010000000

"bitmask to restrict to all code units"
const CODEUNIT_BITS :: UInt64 = (UInt64(1)<<56)-1

"bitmask to test for tiny. Bit set -> (offset,size) pair stored"
const NOTTINY_BIT :: UInt64 = (UInt64(1)<<63)

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
which is not accompanied by a code unit buffer parameter.

# implementation details

Memory layout is 8 bytes, RAM representation as an UInt64 in host format
(this can have consequences for serializing, if data is exchanged
between systems of differing endianness).

If the most significant bit is clear, we have a really tiny string of
up to 7 code units, directly encoded in the TinyToken value in its
bytes 6 down to 0. If size is less than 7, cu[7] .. cu[size+1] must be 0.

The memory layout is similar to the julia memory layout for the Char type,
with the difference that the code unit count is explicitly stored in one byte,
together with category and the "tiny flag".

Order is chosen in a way that a lexikographically correct string compare
for TinyToken-s can be done with one single UInt64 compare operation.

Memory layout:

```
bit 63 63 62 60 59 58 57 56 byte6 byte5 byte4 byte3 byte2 byte1 byte0
    =0 =========== ========
    |  |           |        |     |     |     |     |     |     |
    |  |           |        cu[1] cu[2] cu[3] cu[4] cu[5] cu[6] cu[7]
    |  |           size: 0..7
    |  category: 0..15
    |
    =0: really tiny, 0..7 code units stored directly in byte 6..1
```

If the most significant bit is set, we have a usually larger string of
up to 2^27-1 code units, encoded elsewere in an external code unit buffer.
The TinyToken value stores offset and size information in byte 0..6.

Treatment of the external code unit buffer needs some attention: it is NOT
part of the TinyToken type. Instead, methods operating on TinyToken-s code
units need some buffer supply by its context, usually via an additional
parameter or a closure. Having no buffer information will throw an exception.

Memory layout:

```
bit 63 63 62 60 59 58 57 56 byte6 byte5 byte4 byte3 byte2 byte1 byte0
    =1 =========== ======== ================= =======================
    |  |           |        |                 |
    |  |           |        |                 offset: 0..2^32-1
    |  |           |        size>>3: bits 3..26 of size
    |  |           size&7: lowest 3 bits
    |  category: 0..15
    |
    =1: code units cu[1]..cu[size] stored in buffer[1+offset] .. buffer[1+offset+size]
```


"""
struct TinyToken <: AbstractToken
    bits::UInt64

    """
    lowlevel constructor for "nottiny" token.

    buffer is only supplied to test if parameters are in bounds

    Has UInt64 parameters to reduce chances that someone uses it
    unintentionally.
    """
    function TinyToken(cat::UInt64, offset::UInt64, size::UInt64, buffer:String)
        @boundscheck checkcategory(cat)
        @boundscheck check_ofs_size(buffer, offset, size)

        bsize = ncodeunits(buffer)
        @checkbounds
        @boundscheck checkoffset(offset)
        @boundscheck checksize(size,1<<27-1)
        new (NOTTINY_BIT | cat<<59) | (size&7)<<56 | (size>>3)<<32 | offset)
    end

    """
    constructor for "tiny" string token
    """
    function TinyToken(cat::Unsigned = 0, s::String)
        @boundscheck checkcategory(cat)
        size = ncodeunits(s)
        @boundscheck checksize(size,7)
        new (NOTTINY_BIT | cat<<59) | (size&7)<<56 | (size>>3)<<32 | offset)
    end


    function TinyToken(cat::UInt8, s::String, offset::UInt64, size::Int)
        @boundscheck checkcategory(cat)
        msb = UInt64(cat)<<59 # most significant byte
        if first >= last
            return new(UInt64(cat)<<59) # empty string
        end
        @boundscheck checkbounds(s,first,last)
        size = last-first
        @boundscheck size < 8 || throw BoundsError("Size too large for TinyToken string constructor",size)
        new(bits)
    end


    function TinyToken(c::Char)
        bits ::UInt64 =
          (Base.codelen(c)) << 56) |
          ( <<59 |
          Int64(reinterpret(UInt32, c)) << 24
       new(bits)
   end

    "override default constructor to convert anything to a string token"
    TinyToken (value::Any) = TinyToken (U_STRING,string(value))
    function TinyToken(::AsLatin{true},s::AbstractString)
    end
    function TinyToken(t::AbstractToken)
    end
end

"""
    t"anytext"

returns a TinyToken containing "anytext".

Restriction: argument literal must resolve to a
character sequence of at most 7 code units
"""
macro t_str(s) = TinyToken(s)




function offset(t::TinyToken)
    mask = ((reinterpret(Int64,t.bits)>>63) & (UInt64(2^32)-1) # 0 for direct CU, 0xffffffff else
    convert(UInt32,t.bits & mask)
end

function Base.ncodeunits(t::TinyToken)
    # tricky code without jumps:
    # 3 lowest bits of the size are always stored at bit position 56..58.
    # if NONASCII_BIT is set, we have additional 24 bits for size at bits 32..55
    # We build a mask for bits 32..55 all 1 (bit 63 set, data in buffer)
    # or 0 (bit 63 clear, all data in token, size is 0..7)
    # by arithmetic shift of bit 63. ANDing with t.bits and shift by 29 gives
    # the high bits 3..26 of the size.
    masksize = (reinterpret(Int64,t.bits)>>31) & ((2^24-1)<<32) ## length bitfield mask
    ((t.bits &masksize)>>>29) | ((t.bits >>>56) & 7)
end


function Base.cmp(a::TinyToken, b::TinyToken)
    @boundscheck checktiny(a); checktiny(b)
    # both tiny: compare all code units in one step
    (a.bits& 2^56-1) < (b.bits& (2^56-1)) && return -1
    (a.bits& 2^56-1) > (b.bits& (2^56-1)) && return 1
    0
end



category(t::TinyToken) = UInt8((t.bits>>59)&15)




"throw an error if token references some buffer"
function checktiny(t::TinyToken)
    @boundscheck t.bits&NOTTINY_BIT >=0 || throw(ErrorException("TinyToken references unknown buffer: &t"))
end

"throw an error if token category is out of bounds"
function checkcategory(cat::Unsigned)
    @boundscheck cat <= 15 || throw(ErrorException("Token category too large (max 15): &cat"))
    nothing
end

"throw an error if token offset is out of bounds"
function checkoffset(ofs::Unsigned)
    @boundscheck ofs <= typemax(UInt32) || throw(ErrorException("token offset too large: &ofs"))
    nothing

end

function check_ofs_size(buffer::String, offset, size)
    bsize = ncodeunits(buffer)
    offset >= 0 && offset <= bsize || throw BoundsError(buffer,offset+1)
    size >= 0 && offset+size <= bsize || BoundsError(buffer,offset+size)
end


"throw an error if token references some buffer"
function checksize(size::Unsigned, maxsize=7))
    @boundscheck size <= maxsize || throw(ErrorException("too many code units: &size"))
end


function subtoken(t::T<:AbstractToken, first::Int, last::Int) = T(t,first,last)
    #@boundscheck checktiny(t)
    Int64 ret = t.bits

    TinyToken ret = t
    n = ncodeunits(t)
    if (last<n)

    end

end


Base.show(io::IO, t::TinyToken)
    print(io,'^',category(t))
    if t.bits>=0
        Base.print_quoted(io, t)
    else
        print(io,"$[ofs=",offset(t),",n=",ncodeunits(t),']')
    end
end



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