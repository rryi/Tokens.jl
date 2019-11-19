# TinyToken type and associated methods

"maximal count of code units directly encoded in a TinyToken"
const MAX_TINY_SIZE = 7

"bitmask to check if any inline code unit is not ascii"
const NOTASCII_BITS :: Int64 = 0x10000000100000001000000010000000100000001000000010000000

"bitmask to restrict to all code units"
const CODEUNIT_BITS :: Int64 = (Int64(1)<<56)-1

"bitmask to test for tiny. Bit set -> (offset,size) pair stored"
const NOTTINY_BIT :: Int64 = (Int64(1)<<63)


"marker type to tag constructors as unsafe"
struct Unsafe end


"""
Flyweight string with an associated token category.

This string data type directly stores very short strings with a length
of up to 7 code units. It supports the AbstractString API and can be
used as a memory efficient substitute for String instances, avoiding
any pointer overhead.

Combined with a code unit buffer, it can hold strings of a length below
2**27. The flyweight value encodes an offset and a length, in this case.

TinyToken can be used directly in application code if its length limit
fits the use case. More common is a combination with an additional code unit
buffer. The TinyToken bits pattern is interpreted differently, in this
case, as an offset (32bit) and a length (27bit).

See [`Token`](@ref) and [`MutableToken`](@ref) for an implementation.

# methods without code unit buffer parameter

Strings with up to 7 Utf8 code units can be stored in one TinyToken
instance, without an additional buffer. For most functions using a TinyToken,
there is a method with a TinyToken parameter but no code unit buffer parameter.
There are two reasons why a code unit buffer parameter is not present:

(a) Function does not access code units, like *ncodeunits* or *category*.

(b) Function needs code unit access, but an optimized method for short strings
stored directly in the TinyToken instance is supplied.

In case (b) checks have to be implemented that grant:

  * TinyToken input parameters have stored its code units directly. Please take
    into account, that a number of code units below 8 does not imply that those
    code units are stored directly. You have to check is not sufficient a TinyToken with less than 8 code units can cana length below 8 up to 7 does is not necessarily
    stored directlycan

  * TinyToken results returned without a code unit buffer store its code units
    directly.

Those checks are annoted with *@boundscheck*. This allows for efficient
operation when it is known in advance that the whole operation can be carried
out on TinyToken-s without external code unit buffer, by annotating the calls
with *@inbounds*.

the operation

  It is an optimized version Those methods should

 Size limit is checked unless you
annotate the call with @inbounds.

# use with buffer



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

There are two variants of TinyToken, that one with directly encoded dode units,
and that one referencing an external buffer. The Julia-typical way for an
implementation would be to declare these variants as different types and to
define TinyToken as a union of these variants. The price would be to have one
additional byte per instance for the union overhead. Instead, the variants
are managed internally by the functions working with TinyToken, and type
implementation details are marked as private and hidden to the extend possible
with Julia.

Design goal is to keep an TinyToken instance in one 64bit value.
Memory layout is 8 bytes, RAM representation is an Int64 in host format.
The sign bit (most significant bit) is used to distinguish the two TinyToken
variants.

If the sign bit is clear, we have a really tiny string of
up to 7 code units, directly encoded in the TinyToken value in its
bytes 6 down to 0. If size is less than 7, unused code units must be 0.

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
parameter or a closure. Missing buffer information will cause an exception.

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
    =1: code units stored in buffer[1+offset] .. buffer[1+offset+size]
```


"""
struct TinyToken <: AbstractToken
    bits::Int64
    """
    UNSAFE !! lowlevel constructor for a token referencing some external buffer.

    This constructor is used internally and should not be called from
    application code - NO CHECKS AT ALL are performed, the resulting
    TinyToken might be invalid. The field parameter contains several
    data fields packed into 8 bytes on bit level.
    """
    function TinyToken(::Unsafe, fields::Int64)
        new (fields)
    end
end



"""
UNSAFE !! lowlevel constructor for a token referencing some external buffer.

This constructor is used internally and should not be called from
application code - NO CHECKS AT ALL are performed, the resulting
TinyToken might be invalid.

Has explicit UInt parameters to reduce chances that someone uses it
unintentionally.

In application code, use TinyToken(category,offset,size,s<:AbstractString).
"""

function TinyToken(::Unsafe, category::UInt64, offset::UInt64, size::UInt64)
    TinyToken (Unsafe, NOTTINY_BIT | category<<59 | (size&7)<<56 | (size>>3)<<32 | offset)
end


"""
constructor for "tiny" string token from Utf8-encoded strings
"""
function TinyToken(cat::Unsigned, s::Union{AbstractToken,String,SubString{String})
    @boundscheck checkcategory(cat)
    size = ncodeunits(s)
    @boundscheck checksize(size,7)
    @inbounds TinyToken(UInt64(cat),s, UInt64(0),UInt64(size),s)
end


"""
constructor for "tiny" string token
"""
function TinyToken(cat::Unsigned, s::Union{AbstractToken,String,SubString{String},
                    UInt64 offset, UInt64 size)
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



"""
    t?"shorttext"

returns a TinyToken containing "anytext" with
category ?. ? is a hex character, interpreted as an UInt8

Restriction: argument literal must resolve to a
character sequence of at most 7 code units
"""
macro t0_str(s) = TinyToken(0x0,s)
macro t1_str(s) = TinyToken(0x1,s)
macro t2_str(s) = TinyToken(0x2,s)
macro t3_str(s) = TinyToken(0x3,s)
macro t4_str(s) = TinyToken(0x4,s)
macro t5_str(s) = TinyToken(0x5,s)
macro t6_str(s) = TinyToken(0x6,s)
macro t7_str(s) = TinyToken(0x7,s)
macro t8_str(s) = TinyToken(0x8,s)
macro t9_str(s) = TinyToken(0x9,s)
macro ta_str(s) = TinyToken(0xa,s)
macro tb_str(s) = TinyToken(0xb,s)
macro tc_str(s) = TinyToken(0xc,s)
macro td_str(s) = TinyToken(0xd,s)
macro te_str(s) = TinyToken(0xe,s)
macro tf_str(s) = TinyToken(0xf,s)

"True, if code units are stored directly in TinyToken (no external buffer)"
function checktiny (::Bool, t::TinyToken)
    t.bits >=0
end

function offset(t::TinyToken)
    mask = ((reinterpret(Int64,t.bits)>>63) & (UInt64(1)<<32 -1) # 0 for direct CU, 0xffffffff else
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
    @boundscheck checktiny(Bool,t)wowo || throw(ErrorException("TinyToken references unknown buffer: &t"))
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

"throw an error if token offset is out of bounds"
function check_ofs_size(offset:: Unsigned, size:: Unsigned, limit::Unsigned)
    @boundscheck offset <= limit || throw BoundsError(offset,limit)
    @boundscheck offset+size <= bsize || throw BoundsError(offset+size,limit)
    nothing
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
