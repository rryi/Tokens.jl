# TinyToken type and associated methods

"maximal count of code units directly encoded in a TinyToken"
const MAX_TINY_SIZE = 7

"maximal count of code units in any Token"
const MAX_TOKEN_SIZE = (SIZE_BITS_27 ? (1<<27) : (1<<24) ) - 1

"bitmask to check if any inline code unit is not ascii"
const NOTASCII_BITS :: UInt64 = 0x10000000100000001000000010000000100000001000000010000000

"bitmask to restrict to all code units"
const CODEUNIT_BITS :: UInt64 = (UInt64(1)<<56)-1

"bitmask to test for tiny. Bit set -> (offset,size) pair stored"
const NOTTINY_BIT :: UInt64 = (UInt64(1)<<63)


"marker type to tag constructors as unsafe"
struct Unsafe end



"Known standard types usable in reinterpret for 64 bit types"
const Bits64 = union {UInt64, Int64}

"""
A flyweight data structure for an immutable Token.

All subtypes must be 64 bit primitive data types which implement a certain
bitmap layout and conventions for processing it.

"""
abstract type TinyToken <: AbstractToken



"""
Flyweight token with direct encoding of its code units.

This string data type directly stores very short strings with a length
of up to 7 code units in its value. It supports the AbstractString API and can be
used as a memory efficient substitute for String instances, avoiding
any pointer overhead.

# implementation details

Design goal is to treat a TinyToken instance as a 64 bit primitive value.
To allow bit operations, memory layout is in host endianness like Int64.
Take that into account on binary serialization.
The sign bit (most significant bit) is used to distinguish a DirectToken
value from other TinyToken types. For a TinyToken, it must always be 0.

DirectToken encodes really tiny strings of up to 7 code units, directly
in its 64 bit value. If size is less than 7, unused code units must be 0.

The memory layout is similar to the julia memory layout for the Char type,
with the difference that the code unit count is explicitly stored in a separate
byte, together with category and the "isdirect flag" in the sign bit.

Order is chosen in a way that a lexikographically correct string compare
for DirectToken-s can be done with one single UInt64 compare operation.

Memory layout:

```
bit 63 63 62 60 59 58 57 56 byte6 byte5 byte4 byte3 byte2 byte1 byte0
    == =========== ========
    |  |           |        |     |     |     |     |     |     |
    |  |           |        cu[1] cu[2] cu[3] cu[4] cu[5] cu[6] cu[7]
    |  |           size: 0..7
    |  category: 0..15
    |
    =0: marker flag must be 0 in DirectToken instances
```


"""
primitive type DirectToken <: TinyToken 64 end




"""
Flyweight token accomplished by a code unit buffer.

This token type is a flyweight data structure which needs an accompanying
code unit buffer. In methods, it is either supplied as an additional parameter,
or it is known from the context.

See [`Token`](@ref) and [`MutableToken`](@ref) for an implementation
with explicitly associated buffer.

# methods without code unit buffer parameter

There are two reasons why a code unit buffer parameter is not present:

(a) function does not access code units, like *ncodeunits*, *category*
or *isdirect*.

(b) function needs code unit access, and knows the buffer from its context.


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
    =1: must be 1 for any BufferToken instance.
    code units are stored in buffer[offset+1] .. buffer[offset+size]
    buffer must be known in the processing context.
```


"""
primitive type BufferToken <: TinyToken 64 end



"""
Flyweight token union type.

This token type is an alternative implementation for the julia
construct *union {DirectToken, BufferToken}*.

The sign bit in the 64-bit value (interpreted as UInt64) distinguishes
between the two types that make up the union, instead of a separate
tag byte which would be used by a Julia union construct.

The type acts as a DirectToken or a BufferToken depending on its value,
which is tested at runtime, introducing the overhead of one test and
conditional jump on access. Because caller do not know in advance
(at compiletime) if an instance is of type BufferToken, the buffer needed
by a BufferToken has to be supplied in the processing context and is
unused if the concrete instance is a DirectToken.

The advantage over BufferToken is less memory consumption, and faster
access to code units because no indirection takes place, if the actual
instance is a DirectToken.

Use HypridToken instead of BufferToken in cases where strings with up to
7 code units have a significant proportion. Benchmark both alternatives
if runtime performance is critical - it depends on your operation mix
whether HybridToken gives faster code or not.

[`Token`](@ref) and [`TokenVector`](@ref) are specialized on HybridToken,
variants see its definition for variants with BufferToken.

# methods without code unit buffer parameter

For many functions using a HybridToken, there is a method with a HybridToken
parameter but no code unit buffer parameter.
There are several reasons why a code unit buffer parameter is not present:

(a) function does not access code units, like *ncodeunits*, *category*
or *isdirect*.

(b) function needs code unit access, but knows where its buffer is, from
some context information

(c) function needs code unit access and fails if the HybridToken is not a
DirectToken

# warning on @inbounds usage

Methods that have a HybridToken parameter, need access to its code units,
but have no associated code unit buffer as a parameter, will check if
code units are stored directly in the supplied token. If not, they will
throw a BoundsError.

Those type checks are annoted with *@boundscheck*. This allows for efficient
operation when it is known in advance that the whole operation can be carried
out on HybridToken-s without external code unit buffer, by annotating the calls
with *@inbounds*.

Code annotated with @inbounds assures not only that all used indices are in
bounds. It additionally assures that all code units are directly stored
in the HybridToken instance for all method calls with a HybridToken parameter
which is not accompanied by a code unit buffer parameter.

# implementation details

Because all values of DirectToken and BufferToken, seen as native 64-bit-chunk,
are not overlapping, the concrete type of a HybridToken is easily determined
at runtime (test the sign bit in Int64 interpretation).

ncodeunits can be implemented without conditional jumps, by tricky bit
operations. This is left to llvm optimization - benchmarks have shown that
llvm is capable of doing it and decides on code generation which implementation
is regarded faster.

category bits are identical for DirectToken and BufferToken, so no conditional
code is needed.

"""
primitive type HybridToken <: TinyToken 64 end




"""
Private unsafe convenience converter to an UInt64 value.

All TinyToken operations are finally implemented with native 64 bit
operations defined for Int64 or UInt 64. We need a short notation
to convert between (U)Int64 and TinyToken. No checks!!
"""
uint(t::TinyToken) =  reinterpret(UInt64,t)

"""
Private unsafe convenience converter to an UInt64 value.

All TinyToken operations are finally implemented with native 64 bit
operations defined for Int64 or UInt 64. We need a short notation
to convert between (U)Int64 and TinyToken. No checks!!
"""
dt(bits) = reinterpret(DirectToken,bits)

"""
Private unsafe convenience converter to an UInt64 value.

All TinyToken operations are finally implemented with native 64 bit
operations defined for Int64 or UInt 64. We need a short notation
to convert between (U)Int64 and TinyToken. No checks!!
"""
ht(bits) = reinterpret(HybridToken,bits)

"""
Private unsafe convenience converter to an UInt64 value.

All TinyToken operations are finally implemented with native 64 bit
operations defined for Int64 or UInt 64. We need a short notation
to convert between (U)Int64 and TinyToken. No checks!!
"""
bt(bits) = reinterpret(BufferToken,bits)

"""
UNSAFE !! lowlevel constructor for a token referencing some external buffer.

This constructor is used internally and should not be called from
application code - NO CHECKS AT ALL are performed, the resulting
TinyToken might be invalid.

In application code, use TinyToken(category,offset,size,s<:AbstractString).
"""
function BufferToken(::Unsafe, category::UInt64, offset::UInt64, size::UInt64)
    bt(NOTTINY_BIT | category<<59 | (size&7)<<56 | (size>>3)<<32 | offset)
end


"""
UNSAFE !! lowlevel constructor for a token with direct encoding

This constructor is used internally and should not be called from
application code - NO CHECKS AT ALL are performed, the resulting
TinyToken might be invalid.

In application code, use TinyToken(category,s::Utf8String).
"""
function TinyToken(::Unsafe, category::UInt64, size)
    tt( category<<59 | UInt64(size&7)<<56)
end


"""
constructor for direct encoding from Utf8-encoded strings.

bounds checks: valid category, ncodeunits(s)<=7
"""
function TinyToken(category::Unsigned, s::Utf8String)
    @boundscheck checkcategory(category)
    size = UInt64(ncodeunits(s))
    @boundscheck checksize(size,7)
    @inbounds TinyToken(category, UInt64(0),size,s)
end


TinyToken(cat::Unsigned, s::AbstractString) = TinyToken(cat,string(s))


"subtoken with offset/size descriptor"
TinyToken(offset::Unsigned, size::Unsigned, t::AbstractToken) = TinyToken(
    category(t),offset,size,t)
)




"""
constructor for empty tiny token with direct data
"""
function TinyToken(cat::Unsigned)
    @boundscheck checkcategory(cat)
    reinterpret(TinyToken,UInt64(cat)<<59)ä
end


"""
constructor for direct encoding from Utf8-encoded strings.

bounds checks: valid cat, size<=7, offset+size<=ncodeunits(s)
"""
function TinyToken(cat::Unsigned, offset::Unsigned, size::Unsigned, s::Utf8String)
    @boundscheck checkcategory(cat)
    size = ncodeunits(s)
    @boundscheck checksize(size,7)
    @boundscheck size==0 || checkbounds(s, offset+size)
    tt::UInt64 =
    t = TinyToken(UInt64(cat))
    for i in 1::size
        @inbounds t = t * codeunit(s,offset+i)
    end
    t
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


function TinyToken(cat::UInt8, c::Char)
    @boundscheck checkcategory(cat)
    TinyToken(Unsafe, UInt64(cat<<3 | Base.codelen(c)) << 56
        |  UInt64(reinterpret(UInt32, c)) << 24)
end


"set a codeunit within a bitfield assuming current value is 0. No checks."
unsafe_set(t:TinyToken, index, codeunit::Uint8) =
    reinterpret(TinyToken, uint(t) | ( UInt64(codeunit)<< ((7-index)<<3)

"set a codeunit within a TinyToken assuming current value is 0. No checks."
unsafe_set(t::TinyToken, index, codeunit::Uint8) =
     TinyToken(Unsafe, unsafe_set(t.bits, index, codeunit))

"""
    *(t::TinyToken, s::Union{UInt8,Char,AbstractString)

concatenation has to be supported. category is always copied from base
token (first argument). code units are allowed to be concatenated, this
can result in tokens representing an invalid Utf8 code unit sequence.

Every token implementation must supply UInt8 (code unit) concatenation,
other concatenation arguments have a default implementation using
code unit concatenation
"""
function (*)(t::TinyToken, s::Utf8String)


end

function (*)(t::TinyToken, c::Char)


end


function (*)(t::TinyToken, cu::UInt8)
    @boundscheck checkappend(t,1)
    unsafe_set (append(t,1))

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
    masksize = (reinterpret(Int64,t)>>31) & ((1<<24-1)<<32)
    ((reinterpret(Int64,t)&masksize)>>>29) | ((reinterpret(Int64,t) >>>56) & 7)
end


function Base.cmp(a::TinyToken, b::TinyToken)
    @boundscheck checktiny(a); checktiny(b)
    # both tiny: compare all code units in one step
    (a.bits& 2^56-1) < (b.bits& (2^56-1)) && return -1
    (a.bits& 2^56-1) > (b.bits& (2^56-1)) && return 1
    0
end



category(t::TinyToken) = UInt8((t.bits>>59)&15)


"True, if code units are stored directly in TinyToken (no external buffer)"
function checktiny (::Bool, t::TinyToken)
    reinterpret(Int64,t.bits) >=0
end

"throw an error if token references some buffer"
function checktiny(t::TinyToken)
    @boundscheck checktiny(Bool,t) || throw(ErrorException("TinyToken references unknown buffer: &t"))
    nothing
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


"throw an error if token references some buffer"
function checkappend(t::TinyToken,append))
    @boundscheck checktiny(t)
    @boundscheck ((t.bits>>56 & 7) + append <=7) || throw(ErrorException("no &append bytes left in tinytoken: &t"))
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
