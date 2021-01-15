# FlyToken type and associated methods

"maximal count of code units directly encoded in a FlyToken"
const MAX_DIRECT_SIZE = UInt64(7)

"maximal count of code units in any buffer based Token"
const MAX_TOKEN_SIZE = UInt64((1<<27) - 1)

"bitmask to check if any inline code unit is not ascii"
const NOTASCII_BITS = UInt64(0b10000000100000001000000010000000100000001000000010000000)

"bitmask to restrict to all code units in a DirectFly"
const CODEUNIT_BITS =(UInt64(1)<<56)-1

"bitmask to isolate size bits in DirectFly"
const DIRECT_SIZE_BITS = UInt64(MAX_DIRECT_SIZE)<<56

"bitmask to isolate size bits in BufferFly"
const FLY_SIZE_BITS = UInt64(MAX_TOKEN_SIZE)<<32

"bitmask to isolate splitted size bits in BufferFly"
const SPLIT_SIZE_BITS = UInt64((1<<24)-1)<<32

"bitmask to restrict to all code units in a DirectFly"
const OFFSET_BITS =(UInt64(1)<<32)-1

"bitmask to isolate category and fly type bit in any FlyToken"
const CATEGORY_FLY_BITS = UInt64(31)<<59

"bitmask to isolate category in any FlyToken"
const CATEGORY_BITS = UInt64(15)<<59

"bitmask to test for tiny. Bit set -> (offset,size) pair stored"
const NOTTINY_BIT = (UInt64(1)<<63)

"true: size bitfield is split in [`BufferFly`](@ref)"
const FLY_SPLIT_SIZE = false


"Known standard types usable in reinterpret for 64 bit types"
const Bits64 = Union{UInt64, Int64}


"""
A flyweight data structure for tokens.

All subtypes must be 64 bit primitive data types which implement a certain
bitmap layout and methods for processing it.

FlyToken implementations will throw an error if a Token API method requires
a buffer and no buffer is supplied to the method. Not all AbstractToken API
methods are implemented by all FlyToken subtypes, many FlyToken subtypes
can only be used as  part of a compound token, not "standalone".

"""
abstract type FlyToken <: AbstractToken end



"""
Flyweight token with direct ("inline") encoding of its code units.

This string data type directly stores very short strings with a length
of up to 7 code units in its value. It supports the AbstractString API and can be
used as a memory efficient substitute for String instances, avoiding
any pointer overhead. It implements the full AbstractToken API.

# performance considerations

DirectFly instances consume much less memory, compared to String
instances. They are true values (no heap allocation), which can improve
data locality and thus CPU cache use. Comparison is considerably faster than
String comparison.

Depending on the API used, there is a disadvantage because it is not possible
to obtain a C-stype pointer to the contents of a DirectFly - without
creating a temporary String copy. E. G. hashing suffers from that.

# implementation details

Design goal is to treat a DirectFly instance as a 64 bit primitive value.
To allow bit operations, memory layout is in host endianness like Int64.
Take that into account on binary serialization.
The sign bit (most significant bit) is used to distinguish a DirectFly
value from other FlyToken types. For a DirectFly, it must always be 0.

DirectFly encodes really tiny strings of up to 7 code units, directly
in its 64 bit value. If size is less than 7, unused code units must be 0.

The memory layout is similar to the julia memory layout for the Char type,
with the difference that the code unit count is explicitly stored in a separate
byte, together with category and the "isdirect flag" in the sign bit.

Order is chosen in a way that a lexikographically correct string compare
for DirectFly-s can be done with one single UInt64 compare operation.

Memory layout:

```
bit 63 63 62 60 59 58 57 56 byte6 byte5 byte4 byte3 byte2 byte1 byte0
    == =========== ========
    |  |           |        |     |     |     |     |     |     |
    |  |           |        cu[1] cu[2] cu[3] cu[4] cu[5] cu[6] cu[7]
    |  |           size: 0..7
    |  category: 0..15
    |
    =0: marker flag must be 0 in DirectFly instances
```


"""
primitive type DirectFly <: FlyToken 64 end


"""
Flyweight token accomplished by a code unit buffer.

This token type is a flyweight data structure which needs an accompanying
code unit buffer. Any method which operates on the contents, needs access to
the associated buffer. Usually, the buffer is supplied as an additional
parameter.

BufferFly is considered a module-private type and thus not exported by Tokens
module. It does NOT implement the full AbstractString API! Only
AbstractBuffer and AbstractToken methods which do not need contents access
are implemented.

See [`BToken`](@ref) for an implementation with explicitly associated buffer.



Memory layout:

```
bit 63 63 62 60 59 58 57 56 byte6 byte5 byte4 byte3 byte2 byte1 byte0
    =1 =========== ========================== =======================
    |  |           |                          |
    |  |           |                          offset: 0..2^32-1
    |  |           size: 26 bits
    |  category: 0..15
    |
    =1: must be 1 for any BToken instance.
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
        =1: must be 1 for any BToken instance.
        code units are stored in buffer[offset+1] .. buffer[offset+size]
        buffer must be known in the processing context.
    ```


"""
primitive type BufferFly <: FlyToken 64 end



"""
Flyweight/direct token union type.

This token type is an alternative implementation for the julia
construct *union {DirectFly, BufferFly}*.

The sign bit in the 64-bit value (interpreted as UInt64) distinguishes
between the two types that make up the union, instead of a separate
tag byte which would be used by a Julia union construct.

The type acts as a DirectFly or a BufferFly depending on its value,
which is tested at runtime, introducing the overhead of one test and
conditional jump on access.

HybridFly is considered a private type of module Tokens and thus not exported.
It does NOT implement the full AbstractString API! Only AbstractBuffer and
AbstractToken methods which do not need contents access are implemented.

Use HybridFly instead of BufferFly in cases where strings with up to
7 code units have a significant proportion. Benchmark both alternatives
if runtime performance is critical - it depends on your operation mix
whether HybridFly gives faster code or not.

[`Token`](@ref) and [`TokenVector`](@ref) are using HybridFly.

# implementation details

Because all values of DirectFly and BufferFly, seen as native 64-bit-chunk,
are not overlapping, the concrete type of a HybridFly is easily determined
at runtime (test the sign bit in Int64 interpretation).
Use [`isdirect`](@ref)f or this purpose.

usize can be implemented without conditional jumps, by tricky bit
operations. This is left to llvm optimization - benchmarks have shown that
llvm is capable of doing it and decides on code generation which implementation
is regarded faster.

category bits are identical for DirectFly and BufferFly, so no conditional
code is needed.

"""
primitive type HybridFly <: FlyToken 64 end


## basic helpers ##





"""
Private convenience converter to an UInt64 value.

All FlyToken operations are finally implemented with native 64 bit
operations defined for Int64 or UInt64. We need a short notation
to convert between (U)Int64 and FlyToken. No checks!!
"""
u64(t::FlyToken) =  reinterpret(UInt64,t)

"""
Private unsafe convenience converter to a DirectFly.

Argument must be a 64 bit primitive value.
No checks performed!
"""
df(bits) = reinterpret(DirectFly,bits)

"""
Private unsafe convenience converter to a HybridFly.

Argument must be a 64 bit primitive value.
No checks performed!
"""
hf(bits) = reinterpret(HybridFly,bits)

"""
Private unsafe convenience converter to a BufferFly.

Argument must be a 64 bit primitive value.
No checks performed!
"""
bf(bits) = reinterpret(BufferFly,bits)




"""set a codeunit within a DirectFly assuming current value is 0.

index must be 1..7, t mush have codeunit value 0 at index i.
titled unsafe, because no checks are performed.
"""
function unsafe_setcodeunit(t::DirectFly, index, codeunit::UInt8)
    t | ( UInt64(codeunit)<< ((7-index)<<3))
end

offset(t::BufferFly) = (u64(t) & OFFSET_BITS)%UInt32

offset(t::DirectFly) = UInt32(0)

offset(t::HybridFly) = isdirect(t) ? offset(df(t)) : offset(bf(t))




## constructors  ##



"empty token (offset, length are 0)"
function BufferFly(cat::TCategory)
    bf(NOTTINY_BIT | (cat%UInt64)<<59)
end


"""
token with given size, bounds-checked, offset 0
"""
Base.@propagate_inbounds function BufferFly(cat::TCategory, size::UInt64)
    @boundscheck checksize(size,MAX_TOKEN_SIZE)
    if FLY_SPLIT_SIZE
        BufferFly(cat) | ((size&7)<<56 | (size>>>3)<<32)
    else
        BufferFly(cat) | (size<<32)
    end
end


"""
Lowlevel constructor for a token referencing some external buffer.

UNSAFE because the referenced buffer is not given.
Method cannot check whether (offset,size) is valid for the buffer which is
used with this token. You have to grant (or check) validity of (offset,size)
with respect to the referenced buffer in your code elsewere.

"""
Base.@propagate_inbounds function BufferFly(cat::TCategory, offset::UInt32, size::UInt64)
    BufferFly(cat,size)|offset
end


"""
Constructor of an empty token with given category.

All codeunit bytes are 0, length is 0.
"""
DirectFly(cat::TCategory) = df( UInt64(cat)<<59 )


"token with given size, bounds-checked, offset 0"
Base.@propagate_inbounds function DirectFly(cat::TCategory, size::UInt64)
    @boundscheck checksize(size,MAX_DIRECT_SIZE)
    df(u64(DirectFly(cat)) | size<<56)
end



"Construct a DirectFly for size beliw 8 else a BufferFly"
Base.@propagate_inbounds function HybridFly(cat::TCategory, size::UInt64)
    size <= MAX_DIRECT_SIZE ?
        hf(DirectFly(cat,size)) :
        hf(BufferFly(cat,size))
end


"""
constructor for direct encoding from Utf8-encoded strings.

bounds checks performed
"""
function DirectFly(cat::TCategory, offset::UInt32, size::UInt64, s::Utf8String)
    size == 0 && return DirectFly(cat)
    @boundscheck checkbounds(s,offset+size)
    fly = DirectFly(cat,size)
    for i in 1:size
        fly = unsafe_setcodeunit(fly,i,codeunit(s,offset+i))
    end
    fly
end

"token from a string constant (requires size<8)"
function DirectFly(cat::TCategory, s::Utf8String)
    size = usize(s)
    @boundscheck checksize(size,MAX_DIRECT_SIZE)
    fly = DirectFly(cat,size)
    for i in 1:size
        fly = unsafe_setcodeunit(fly,i,codeunit(s,i))
    end
    fly
end


function DirectFly(cat::TCategory, c::Char)
    df(u64(DirectFly(cat,Base.codelen(c))) | UInt64(reinterpret(UInt32, c)) << 24)
end


## Token API methods ##

category(t::FlyToken) = reinterpret(TCategory, UInt8((u64(t) >>>59)&15))

category(t::DirectFly) = reinterpret(TCategory, UInt8(u64(t) >>>59))

isdirect(t::HybridFly) = reinterpret(Int64,t)>=0

isdirect(t::DirectFly) = true

isdirect(t::BufferFly) = false

usize(t::DirectFly) = (u64(t)>>56) & MAX_DIRECT_SIZE

function usize(t::BufferFly)
    if FLY_SPLIT_SIZE
        usize(df(t)) | (u64(t)&SPLIT_SIZE_BITS)>>29
    else
        (u64(t)&FLY_SIZE_BITS)>>32
    end
end


function usize(t::HybridFly)
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
            usize(df(t))
        else
            usize(bf(t))
        end
    end
end


"""
    read(io:, t::DirectFly)::DirectFly

read the string portion of a DirectFly
into a DirectFly which already has category and size.
"""

"read the string portion of a DirectFly"
function tread(io::IO, t::DirectFly) ::DirectFly
    for i in 1:usize(t)
        t = unsafe_setcodeunit(t,i,read(io,UInt8))
    end
    t
end


"""
    read(io::IO)::HybridFly

read category and size in HybridFly bit layout.
Uses variable length encoding optimized for small sizes (<=7, <=2047)
returned HybridFly is a DirectFly if size<=7, else a BufferFly.

This is an internal, private helper method.
It does NOT read any offset or content data!
"""
function read(io::IO,::Type{HybridFly})
    b = read(io,Int8)
    if b >= 0 # implies size <= 7
        return hf((b%UInt64)<<56)
    end
    size = (b & MAX_DIRECT_SIZE) % UInt64
    size |=  (read(io,UInt8)%UInt32) << 3
    if size <= MAX_DIRECT_SIZE
        # read 3 more bytes
        size |=  (read(io,UInt8)%UInt32) << 3
        size |=  (read(io,UInt8)%UInt32) << 11
        size |=  (read(io,UInt8)%UInt32) << 19
    end
    return hf(BufferFly(TCategory((b>>3)%UInt8 &0x0f),size))
end


"""
    write(io::IO, t::HybridFly)

write category and size from HybridFly.
variable length writing optimized for small sizes (<=7, <=2047)
Expects a "real" HybridFly, i.e. it is allowed to have a
BufferFly with size <=7.

This is an internal, private helper method.
It does NOT write any offset or content data!
"""
function Base.write(io::IO, t::HybridFly)
    if isdirect(t)
        write(io,(uint(t)>>56)%UInt8)
        return nothing
    end
    size = usize(t)
    b = (t>>56)%UInt8  | (size&MAX_DIRECT_SIZE)%UInt8
    if size<=MAX_DIRECT_SIZE
        # token is not direct, but has compact format
        write(io,b&0x7f)
        return nothing
    end
    write(io,b) # large size bit is set because t not direct
    if size<2048
        # 2-byte-format
        write(io,(size>>3)%UInt8) # is !=0 because size>7
        return nothing
    end
    # 4-byte-format
    write(io,(size>>3)%UInt8)
    write(io,(size>>11)%UInt8)
    write(io,(size>>19)%UInt8)
    nothing
end




## Base operators and functions overloading ##

"bitwise OR applied to a FlyToken."
Base.:|(f::F, orValue::T) where {F<:FlyToken,T<:Unsigned} =
    reinterpret(F,u64(f) | orValue)

"bitwise AND applied to a FlyToken."
Base.:&(f::F, andValue::T) where {F<:FlyToken,T<:Unsigned} =
        reinterpret(F,u64(f) & andValue)

# serializing of DirectFly
function Base.write(io::IO, t::DirectFly)
    b = (u64(t)>> 56) % UInt8
    write(io,hf(t))
    for i in 1:usize(t)
        write(io, @inbounds codeunit(t,i))
    end
end


function Base.read(io::IO, ::Type{DirectFly})
    t = df(read(io,HybridFly))
    @boundscheck isdirect(t) || boundserror("size too long for DirectFly")
    tread(io,t)
end


function Base.codeunit(t::DirectFly, index::Int)
    @boundscheck checkbounds(t,index)
    (255%UInt8) & (u64(t) >>> ((7-index)<<3))%UInt8
end


function Base.codeunit(t::FlyToken, index::Int)
    isdirect(t) || error("code unit buffer not available in codeunit($t,$index)")
    codeunit(df(t),index)
end


function Base.show(io::IO,t::FlyToken)
    print(io,category(t))
    if isdirect(t)
        Base.print_quoted(io, t)
    else
        print(io,'<',offset(t),',',sizeof(t),'>')
    end
end


"generic comparison by code units - used e.g. for DirectFly"
function cmp_codeunits(a::Utf8String, b::Utf8String)
    al, bl = sizeof(a), sizeof(b)
    ml = min(al, bl)
    i = 1
    @inbounds while i<=ml
        ai = codeunit(a,i)
        bi = codeunit(b,i)
        ai < bi && return -1
        ai > bi && return 1
        i += 1
    end
    al < bl && return -1
    al > bl && return 1
    0
end


function Base.cmp(a::DirectFly, b::DirectFly)
    # both tiny: compare all code units in one step
    (u64(a)&CODEUNIT_BITS) < (u64(b)&CODEUNIT_BITS) && return -1
    (u64(a)&CODEUNIT_BITS) > (u64(b)&CODEUNIT_BITS) && return 1
    0
end


function Base.:(==)(a::DirectFly, b::DirectFly)
    # both tiny: compare all code units in one step
    (u64(a)&CODEUNIT_BITS) == (u64(b)&CODEUNIT_BITS)
end

# other fly tokens have no hash
function Base.hash(t::DirectFly, h::UInt)
    h += Base.memhash_seed
    # note: use pointer(s) here (see #6058).
    mmhash(t, h % UInt32) + h
end

# MurmurHash3 up to 8 bytes
#
function mmhash(str::DirectFly, seed::UInt32)
    h1 = h2 = seed%UInt64
    len = usize(str)
    k1 = 0%UInt64
    if len != 0
        k1 = Base.bswap(u64(str)<<8)
    end
    h1 = mhtail1(h1, k1)
    mhfin(len, h1, h2)
end


#########################################################
################ check methods ##########################
#########################################################


"throw an error if token references some buffer"
Base.@propagate_inbounds function checksize(size::Unsigned, maxsize)
    @boundscheck size <= maxsize ||
        throw(ErrorException("size beyond limit: $size > $maxsize"))
    nothing
end


#=


Base.lastindex(s::FlyToken) = Int(s.size_content & 0xf)
Base.iterate(s::FlyToken, i::Integer) = iterate(String(s), i)
Base.iterate(s::FlyToken) = iterate(String(s))
Base.print(s::FlyToken) = print(String(s))
Base.display(s::FlyToken) = display(String(s))
Base.convert(::FlyToken{T}, s::String) where T = FlyToken{T}(s)
Base.convert(::String, ss::FlyToken) = String(a) #reduce(*, ss)
Base.firstindex(::FlyToken) = 1
Base.collect(s::FlyToken) = getindex.(s, 1:lastindex(s))

==(s::FlyToken, b::String) = begin
    String(s)  == b
end
=#
