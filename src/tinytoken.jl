# TinyToken type and associated methods

"maximal count of code units directly encoded in a TinyToken"
const MAX_DIRECT_SIZE = UInt64(7)

"maximal count of code units in any buffer based Token"
const MAX_TOKEN_SIZE = UInt64((1<<27) - 1)

"bitmask to check if any inline code unit is not ascii"
const NOTASCII_BITS = UInt64(0b10000000100000001000000010000000100000001000000010000000)

"bitmask to restrict to all code units in a DirectToken"
const CODEUNIT_BITS =(UInt64(1)<<56)-1

"bitmask to isolate size bits in DirectToken"
const DIRECT_SIZE_BITS = UInt64(MAX_DIRECT_SIZE)<<56

"bitmask to isolate size bits in FlyToken"
const FLY_SIZE_BITS = UInt64(MAX_TOKEN_SIZE)<<32

"bitmask to isolate splitted size bits in FlyToken"
const SPLIT_SIZE_BITS = UInt64((1<<24)-1)<<32

"bitmask to restrict to all code units in a DirectToken"
const OFFSET_BITS =(UInt64(1)<<32)-1

"bitmask to isolate category and fly type bit in any TinyToken"
const CATEGORY_FLY_BITS = UInt64(31)<<59

"bitmask to isolate category in any TinyToken"
const CATEGORY_BITS = UInt64(15)<<59

"bitmask to test for tiny. Bit set -> (offset,size) pair stored"
const NOTTINY_BIT = (UInt64(1)<<63)

"true: size bitfield is split in [`FlyToken`](@ref)"
const FLY_SPLIT_SIZE = false

"marker type to tag constructors as unsafe"
struct Unsafe end



"Known standard types usable in reinterpret for 64 bit types"
const Bits64 = Union{UInt64, Int64}

"""
A flyweight data structure for an immutable Token.

All subtypes must be 64 bit primitive data types which implement a certain
bitmap layout and methods for processing it.

TinyToken implementations may throw an error if a Token API method requires
a buffer and no buffer is supplied to the method

"""
abstract type TinyToken <: AbstractToken end



"""
Flyweight token with direct ("inline") encoding of its code units.

This string data type directly stores very short strings with a length
of up to 7 code units in its value. It supports the AbstractString API and can be
used as a memory efficient substitute for String instances, avoiding
any pointer overhead.

# performance considerations

DirectToken instances consume much less memory, compared to String
instances. They are true values (no heap allocation), which can improve
data locality and thus CPU cache use. Comparison is considerably faster than
String comparison.

Depending on the API used, there is a disadvantage because it is not possible
to obtain a C-stype pointer to the contents of a DirectToken - without
creating a temporary String copy. E. G. hashing suffers from that.

# implementation details

Design goal is to treat a DirectToken instance as a 64 bit primitive value.
To allow bit operations, memory layout is in host endianness like Int64.
Take that into account on binary serialization.
The sign bit (most significant bit) is used to distinguish a DirectToken
value from other TinyToken types. For a DirectToken, it must always be 0.

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
code unit buffer. Any method which operates on the contents, needs access to
the associated buffer. Usually, the buffer is supplied as an additional
parameter.

FlyToken is considered a module-private type and thus not exported by Tokens
module. It does NOT implement the full AbstractString API! Only
AbstractBuffer and AbstractToken methods which do not need contents access
are implemented.

See [`BufferToken`](@ref) for an implementation with explicitly associated buffer.



Memory layout:

```
bit 63 63 62 60 59 58 57 56 byte6 byte5 byte4 byte3 byte2 byte1 byte0
    =1 =========== ========================== =======================
    |  |           |                          |
    |  |           |                          offset: 0..2^32-1
    |  |           size: 26 bits
    |  category: 0..15
    |
    =1: must be 1 for any BufferToken instance.
    code units are stored in buffer[offset+1] .. buffer[offset+size]
    buffer must be known in the processing context.
```


if [`FLY_SPLIT_SIZE`](@ref) is true, the following bit layout is used:

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
primitive type FlyToken <: TinyToken 64 end



"""
Flyweight/direct token union type.

This token type is an alternative implementation for the julia
construct *union {DirectToken, FlyToken}*.

The sign bit in the 64-bit value (interpreted as UInt64) distinguishes
between the two types that make up the union, instead of a separate
tag byte which would be used by a Julia union construct.

The type acts as a DirectToken or a FlyToken depending on its value,
which is tested at runtime, introducing the overhead of one test and
conditional jump on access.

HybridToken is considered a private type of module Tokens and thus not exported.
It does NOT implement the full AbstractString API! Only AbstractBuffer and
AbstractToken methods which do not need contents access are implemented.


Use HybridToken instead of FlyToken in cases where strings with up to
7 code units have a significant proportion. Benchmark both alternatives
if runtime performance is critical - it depends on your operation mix
whether HybridToken gives faster code or not.

[`Token`](@ref) and [`TokenVector`](@ref) are using HybridToken.

# implementation details

Because all values of DirectToken and BufferToken, seen as native 64-bit-chunk,
are not overlapping, the concrete type of a HybridToken is easily determined
at runtime (test the sign bit in Int64 interpretation).
Use [`isdirect`](@ref)f or this purpose.

usize can be implemented without conditional jumps, by tricky bit
operations. This is left to llvm optimization - benchmarks have shown that
llvm is capable of doing it and decides on code generation which implementation
is regarded faster.

category bits are identical for DirectToken and BufferToken, so no conditional
code is needed.

"""
primitive type HybridToken <: TinyToken 64 end


## basic helpers ##





"""
Private convenience converter to an UInt64 value.

All TinyToken operations are finally implemented with native 64 bit
operations defined for Int64 or UInt64. We need a short notation
to convert between (U)Int64 and TinyToken. No checks!!
"""
u64(t::TinyToken) =  reinterpret(UInt64,t)

"""
Private unsafe convenience converter to a DirectToken.

Argument must be a 64 bit primitive value.
No checks performed!
"""
dt(bits) = reinterpret(DirectToken,bits)

"""
Private unsafe convenience converter to a HybridToken.

Argument must be a 64 bit primitive value.
No checks performed!
"""
ht(bits) = reinterpret(HybridToken,bits)

"""
Private unsafe convenience converter to a FlyToken.

Argument must be a 64 bit primitive value.
No checks performed!
"""
ft(bits) = reinterpret(FlyToken,bits)




"""set a codeunit within a DirectToken assuming current value is 0.

index must be 1..7, no checks performed.
"""
function unsafe_setcodeunit(t::DirectToken, index, codeunit::UInt8)
    dt( u64(t) | ( UInt64(codeunit)<< ((7-index)<<3)))
end

offset(t::FlyToken) = (u64(t) & OFFSET_BITS)%UInt32

offset(t::DirectToken) = UInt32(0)

offset(t::HybridToken) = isdirect(t) ? offset(dt(t)) : offset(ft(t))




## constructors  ##



"empty token (offset, length are 0)"
function FlyToken(cat::TCategory)
#    ft(NOTTINY_BIT | UInt64(cat)<<59)
    ft(NOTTINY_BIT | (cat%UInt64)<<59)
end


"""
token with given size, bounds-checked, offset 0
"""
Base.@propagate_inbounds function FlyToken(cat::TCategory, size::UInt64)
    @boundscheck checksize(size,MAX_TOKEN_SIZE)
    if FLY_SPLIT_SIZE
        ft((size&7)<<56 | (size>>>3)<<32 | u64(FlyToken(cat)))
    else
        ft(size<<32 | u64(FlyToken(cat)))
    end
end


"""
Lowlevel constructor for a token referencing some external buffer.

UNSAFE because the referenced buffer is not given.
Method cannot check whether (offset,size) is valid for the buffer which is
used with this token. You have to grant (or check) validity of (offset,size)
with respect to the referenced buffer in your code elsewere.

"""
Base.@propagate_inbounds function FlyToken(cat::TCategory, offset::UInt32, size::UInt64)
    ft(u64(FlyToken(t),size)|offset)
end


"""
Constructor of an empty token with given category.

All codeunit bytes are 0, length is 0.
"""
DirectToken(cat::TCategory) = dt( UInt64(cat)<<59 )


"token with given size, bounds-checked, offset 0"
Base.@propagate_inbounds function DirectToken(cat::TCategory, size::UInt64)
    @boundscheck checksize(size,MAX_DIRECT_SIZE)
    dt(u64(DirectToken(cat)) | size<<56)
end


"""
constructor for direct encoding from Utf8-encoded strings.

bounds checks performed
"""
function DirectToken(cat::TCategory, offset::UInt32, size::UInt64, s::Utf8String)
    @boundscheck check_ofs_size(offset,size,ncodeunits(s))
    buffer = DirectToken(cat,size)
    for i in 1:size
        buffer = unsafe_setcodeunit(buffer,i,codeunit(s,offset+i))
    end
    buffer
end

"token from a string constant (requires size<8)"
function DirectToken(cat::TCategory, s::Utf8String)
    size = ncodeunits(s)
    @boundscheck check_size(size,MAX_DIRECT_SIZE)
    buffer = DirectToken(cat,size%UInt64)
    for i in 1:size
        buffer = unsafe_setcodeunit(buffer,i,codeunit(s,i))
    end
    buffer
end


function DirectToken(cat::TCategory, c::Char)
    dt(u64(DirectToken(cat,Base.codelen(c))) | UInt64(reinterpret(UInt32, c)) << 24)
end


## Token API methods ##

category(t::TinyToken) = reinterpret(TCategory, UInt8((u64(t) >>>59)&15))

category(t::DirectToken) = reinterpret(TCategory, UInt8(u64(t) >>>59))

isdirect(t::TinyToken) = reinterpret(Int64,t)>=0

isdirect(t::DirectToken) = true

isdirect(t::FlyToken) = false

usize(t::DirectToken) = (u64(t)>>56) & MAX_DIRECT_SIZE

function usize(t::FlyToken)
    if FLY_SPLIT_SIZE
        usize(dt(t)) | (u64(t)&SPLIT_SIZE_BITS)>>29
    else
        (u64(t)&FLY_SIZE_BITS)>>32
    end
end


function usize(t::HybridToken)
    if FLY_SPLIT_SIZE
        # tricky code without jumps:
        # 3 lowest bits of the size are always stored at bit position 56..58.
        # if NONASCII_BIT is set, we have additional 24 bits for size at bits 32..55
        # We build a mask for bits 32..55 all 1 (bit 63 set, data in buffer)
        # or 0 (bit 63 clear, all data in token, size is 0..7)
        # by arithmetic shift of bit 63. ANDing with t.bits and shift by 29 gives
        # the high bits 3..26 of the size.
        masksize = (reinterpret(Int64,t)>>31) & ((1<<24-1)<<32)
        ((u64(t)&masksize)>>>29) | ((u64(t) >>>56) & 7)
    else
        if isdirect(t)
            usize(dt(t))
        else
            usize(ft(t))
        end
    end
end


"read the string portion of a DirectToken"
function _readdirecttoken(io::IO, t::DirectToken) ::DirectToken
    for i in 1:usize(t)
        t = unsafe_setcodeunit(t,i,read(io,UInt8))
    end
    t
end


"read category and size in packed format 1 or 4 bytes"
function _readtokenheader(io::IO)
    b = read(io,UInt8)
    cat = reinterpret(TCategory,((b>>3)&15)%UInt8)
    size = (b & MAX_DIRECT_SIZE) % UInt64
    if b > 127
        size |=  (read(io,UInt8)%UInt64) << 3
        size |=  (read(io,UInt8)%UInt64) << 11
        size |=  (read(io,UInt8)%UInt64) << 19
    end
    (cat,size)
end

"write category and size in packed format 1 or 4 bytes"
function _writetokenheader(io::IO,t::HybridToken)
    size = usize(t)
    b ::UInt8 = UInt8(category(t))<<3 + (size&MAX_DIRECT_SIZE)%UInt8
    write(io,b)
    if size > MAX_DIRECT_SIZE
        size >>= 3
        write(io, size % UInt8)
        size >>= 8
        write(io, size % UInt8)
        size >>= 8
        write(io, size % UInt8)
        size >>= 8
        write(io, size % UInt8)
    end
    nothing
end






## Base operators and functions overloading ##



# serializing of DirectToken
function Base.write(io::IO, t::DirectToken)
    b = (u64(t)>> 56) % UInt8
    write(io,b)
    for i in 1:usize(t)
        write(io, @inbounds codeunit(t,i))
    end
end


function Base.read(io::IO, ::Type{DirectToken})
    cat,size = _readtokenheader(io)
    @boundscheck checksize(size, MAX_DIRECT_SIZE)
    _readdirecttoken(io,DirectToken(cat,size))
end


function Base.codeunit(t::DirectToken, index::Int)
    @boundscheck checkbounds(t,index)
    (255%UInt8) & (u64(t) >>> ((7-index)<<3))%UInt8
end


function Base.codeunit(t::TinyToken, index::Int)
    isdirect(t) || error("code unit buffer not available in codeunit($t,$index)")
    codeunit(dt(t),index)
end


function Base.show(io::IO,t::TinyToken)
    print(io,category(t))
    if isdirect(t)
        Base.print_quoted(io, t)
    else
        print(io,'<',offset(t),',',sizeof(t),'>')
    end
end


function Base.cmp(a::DirectToken, b::DirectToken)
    # both tiny: compare all code units in one step
    (u64(a)&CODEUNIT_BITS) < (u64(b)&CODEUNIT_BITS) && return -1
    (u64(a)&CODEUNIT_BITS) > (u64(b)&CODEUNIT_BITS) && return 1
    0
end

function Base.hash(s::DirectToken, h::UInt)
    h += Base.memhash_seed
    # note: use pointer(s) here (see #6058).
    mmhash(s, h % UInt32) + h
end

# MurmurHash3 up to 8 bytes
#
function mmhash(str::DirectToken, seed::UInt32)
    h1 = h2 = seed%UInt64
    len = usize(str)
    k1 = 0%UInt64
    if len != 0
        k1 = Base.bswap(u64(str)<<8)
    end
    h1 = mhtail1(h1, k1)
    mhfin(len, h1, h2)
end


# GEHT NICHT offset ist sinnlos in serialisierung
#=
# serialize tinytoken as UInt64
function Base.write(io::IO, t::TinyToken)
    write(io,u64(t))
end


# deserialize tinytoken
#= explicit...
Base.read(io::IO, ::FlyToken) = ft(read(io,UInt64))
Base.read(io::IO, ::DirectToken) = dt(read(io,UInt64))
Base.read(io::IO, ::HybridToken) = ht(read(io,UInt64))
=#
Base.read(io::IO, ::Type{T}) where T<: TinyToken = reinterpret(T,read(io,UInt64))
=#



#########################################################
################ check methods ##########################
#########################################################



"throw an error if token references some buffer"
function checkdirect(t::HybridToken)
    @boundscheck isdirect(t) ||
        error("required DirectToken, but HybridToken is no DirectToken: $t")
    nothing
end


"""
throw an error if token offset+size is out of bounds.

limit is the total size if  of the buffer (offset,size) points into
"""
Base.@propagate_inbounds function check_ofs_size(offset:: UInt32, size:: UInt64, limit)
#function check_ofs_size(offset:: UInt32, size:: UInt64, limit)
    @boundscheck checksize(size+offset,limit)
    nothing
end

Base.@propagate_inbounds function check_ofs_size(offset:: UInt32, size:: UInt64, s::AbstractString)
    @boundscheck check_ofs_size(offset,size,ncodeunits(s))
    nothing
end


"throw an error if token references some buffer"
Base.@propagate_inbounds function checksize(size::Unsigned, maxsize)
    @boundscheck size <= maxsize ||
        throw(ErrorException("size beyond limit: $size > $maxsize"))
    nothing
end


#=


Base.lastindex(s::TinyToken) = Int(s.size_content & 0xf)
Base.iterate(s::TinyToken, i::Integer) = iterate(String(s), i)
Base.iterate(s::TinyToken) = iterate(String(s))
Base.print(s::TinyToken) = print(String(s))
Base.display(s::TinyToken) = display(String(s))
Base.convert(::TinyToken{T}, s::String) where T = TinyToken{T}(s)
Base.convert(::String, ss::TinyToken) = String(a) #reduce(*, ss)
Base.firstindex(::TinyToken) = 1
Base.collect(s::TinyToken) = getindex.(s, 1:lastindex(s))

==(s::TinyToken, b::String) = begin
    String(s)  == b
end
=#
