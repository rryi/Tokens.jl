# FlyToken type and associated methods

"maximal count of code units directly encoded in a FlyToken"
const MAX_DIRECT_SIZE = 7%UInt64

"maximal count of code units in any buffer based Token"
const MAX_TOKEN_SIZE = ((1<<27) - 1) %UInt64

"bitmask to check if any inline code unit is not ascii"
const NOTASCII_BITS = UInt64(0b10000000100000001000000010000000100000001000000010000000)

"bitmask to restrict to all code units in a DirectFly"
const CODEUNIT_BITS =((1<<56)-1)%UInt64

"bitmask to isolate size bits in DirectFly"
const DIRECT_SIZE_BITS = MAX_DIRECT_SIZE<<56

"bitmask to isolate size bits in BufferFly"
const BUFFER_SIZE_BITS = MAX_TOKEN_SIZE<<32

"bitmask to isolate splitted size bits in BufferFly"
const SPLIT_SIZE_BITS = (((1<<24)-1)<<32)%UInt64

"bitmask to restrict to all code units in a DirectFly"
const OFFSET_BITS =((1<<32)-1)%UInt64

"bitmask to isolate category and fly type bit in any FlyToken"
const CATEGORY_FLY_BITS = (31<<59)%UInt64

"bitmask to isolate category in any FlyToken"
const CATEGORY_BITS = (15%UInt64)<<59

"bitmask to test for tiny. Bit set -> (offset,size) pair stored"
const NOTDIRECT_BIT = (1%UInt64)<<63

"true: size bitfield is split in [`BufferFly`](@ref)"
const BUFFER_SPLIT_SIZE = false


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

"alias name similar to HToken and BToken"
const DToken = DirectFly

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
    |  |           |                          offset: 32 bits
    |  |           size: 26 bits
    |  category: 0..15
    |
    =1: must be 1 for any BToken instance if size>0 or offset>0
    code units are stored in buffer[offset+1] .. buffer[offset+size]
    buffer must be known in the processing context.
```


if [`BUFFER_SPLIT_SIZE`](@ref) is true, the following bit layout is used:

    ```
    bit 63 63 62 60 59 58 57 56 byte6 byte5 byte4 byte3 byte2 byte1 byte0
        =1 =========== ======== ================= =======================
        |  |           |        |                 |
        |  |           |        |                 offset: 0..2^32-1
        |  |           |        size>>3: bits 3..26 of size
        |  |           size&7: lowest 3 bits
        |  category: 0..15
        |
        =1: must be 1 for any BToken instance if size>0 or offset>0
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

If offset and size bitfields are both 0, DirectFly and BufferFly encode
the same content: an empty string. 

TODO unique nothing/missing encoding with nondirect flag cleared even for BufferFly?! 

TODO we have unnormalized values which may NEVER occur with proper constructor calls:
length==0, but offset bytes !=0. Currently, not used. Could be used to store
an Int32 or a Float in binary format.
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
function BufferFly(cat::Nibble)
    bf(NOTDIRECT_BIT | (cat%UInt64)<<59)
end


"""
token with given size, bounds-checked, offset 0
"""
Base.@propagate_inbounds function BufferFly(cat::Nibble, size::UInt64)
    @boundscheck checkulimit(size,MAX_TOKEN_SIZE)
    if BUFFER_SPLIT_SIZE
        BufferFly(cat) | ((size&7)<<56 | (size>>>3)<<32)
    else
        BufferFly(cat) | (size<<32)
    end
end



"""
raw incomplete token with category and size in packed format, offset 0
"""
function BufferFly(cat_size::Packed31)
    u = NOTDIRECT_BIT | (cat_size%UInt64) <<59 # tricky: all bits above category are shifted out or set by NOTTINYBIT
    s = bits4_30(cat_size)%UInt64
    u |= BUFFER_SPLIT_SIZE ? ((s&7)<<56) | ((s>>>3)<<32)  :  (s<<32)
    bf(u)
end


"raw DirectFly with all content bytes 0"
Base.@propagate_inbounds function DirectFly(cat_size::Packed31)
    u = ((cat_size%UInt64) <<59) & CATEGORY_BITS
    s = bits4_30(cat_size)%UInt64
    @boundscheck checkulimit(s,MAX_DIRECT_SIZE)
    u |= ((s&7)<<56)
    df(u)
end


"raw Hybridfly: direct if size <8"
function HybridFly(cat_size::Packed31)
    @inbounds hf( UInt32(cat_size)<= 0x0000007F ? DirectFly(cat_size) : BufferFly(cat_size))
end



"""
Lowlevel constructor for a token referencing some external buffer.

UNSAFE because the referenced buffer is not given.
Method cannot check whether (offset,size) is valid for the buffer which is
used with this token. You have to grant (or check) validity of (offset,size)
with respect to the referenced buffer in your code elsewere.

"""
Base.@propagate_inbounds function BufferFly(cat::Nibble, offset::UInt32, size::UInt64)
    BufferFly(cat,size)|offset
end


"""
Constructor of an empty token with given category.

All codeunit bytes are 0, length is 0.
"""
DirectFly(cat::Nibble) = df( UInt64(cat)<<59 )


"token with given size, bounds-checked, all code units are 0, prepared for unsafe_setcodeunit"
Base.@propagate_inbounds function DirectFly(cat::Nibble, size::UInt64)
    @boundscheck checkulimit(size,MAX_DIRECT_SIZE)
    df(DirectFly(cat)) | (size << 56)
end



"Construct a DirectFly for size below 8, else a BufferFly with offset 0"
Base.@propagate_inbounds function HybridFly(cat::Nibble, size::UInt64)
    size <= MAX_DIRECT_SIZE ?
        hf(DirectFly(cat,size)) :
        hf(BufferFly(cat,size))
end


"""
constructor for direct encoding from Utf8-encoded strings.

bounds checks performed
"""
Base.@propagate_inbounds function DirectFly(cat::Nibble, offset::UInt32, size::UInt64, s::Utf8String)
    size == 0 && return DirectFly(cat)
    @boundscheck checkbounds(s,offset+size)
    fly = DirectFly(cat,size)
    for i in 1:size
        fly = unsafe_setcodeunit(fly,i,codeunit(s,offset+i))
    end
    fly
end

"token from a string constant (requires size<8)"
function DirectFly(cat::Nibble, s::Utf8String)
    size = usize(s)
    @boundscheck checkulimit(size,MAX_DIRECT_SIZE)
    fly = DirectFly(cat,size)
    for i in 1:size
        fly = unsafe_setcodeunit(fly,i,codeunit(s,i))
    end
    fly
end


function DirectFly(cat::Nibble, c::Char)
    df(u64(DirectFly(cat,Base.codelen(c))) | UInt64(reinterpret(UInt32, c)) << 24)
end





## special values for missing and nothing

const DIRECT_NOTHING = DirectFly(Nibble(T_END))
const DIRECT_MISSING = DirectFly(Nibble(T_SPECIAL))
Base.isnothing(t::FlyToken) = t & (DIRECT_SIZE_BITS | CATEGORY_BITS) == DIRECT_NOTHING
Base.ismissing(t::FlyToken) = t & (DIRECT_SIZE_BITS | CATEGORY_BITS) == DIRECT_MISSING
DirectFly(::Nothing) = DIRECT_NOTHING
DirectFly(::Missing) = DIRECT_MISSING
HybridFly(::Nothing) = hf(DIRECT_NOTHING)
HybridFly(::Missing) = hf(DIRECT_MISSING)
BufferFly(::Nothing) = bf(DIRECT_NOTHING|NOTDIRECT_BIT)
BufferFly(::Missing) = bf(DIRECT_MISSING|NOTDIRECT_BIT)

Base.@propagate_inbounds DirectFly(offset::UInt32, size::UInt64,t::AbstractToken) = DirectFly(t.cat,offset,size,t)


## Token API methods ##

category(t::FlyToken) = reinterpret(Nibble, UInt8((u64(t) >>>59)&15))

category(t::DirectFly) = reinterpret(Nibble, UInt8(u64(t) >>>59))

isdirect(t::HybridFly) = reinterpret(Int64,t)>=0

isdirect(t::DirectFly) = true

isdirect(t::BufferFly) = false

usize(t::DirectFly) = (u64(t)>>56) & MAX_DIRECT_SIZE

function usize(t::BufferFly)
    if BUFFER_SPLIT_SIZE
        usize(df(t)) | (u64(t)&SPLIT_SIZE_BITS)>>29
    else
        (u64(t)&BUFFER_SIZE_BITS)>>32
    end
end


function usize(t::HybridFly)
    if BUFFER_SPLIT_SIZE
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


## helpers for IO
Packed31(f::FlyToken)=Packed31(category(f)%UInt8, f.len%UInt32)

## Base operators and functions overloading ##


function Base.String(d::DirectFly)
    size = d.len
    s = Base._string_n(size)
    p = pointer(s)
    GC.@preserve s begin
        for i in 1::size 
            @inbounds unsafe_store!(p,codeunit(d,i))
            p += 1
        end
    end
    return s
end

"bitwise OR applied to a FlyToken. DANGEROUS: no validity checks!!"
Base.:|(f::F, orValue::T) where {F<:FlyToken,T<:Unsigned} = reinterpret(F,u64(f) | orValue)

"bitwise AND applied to a FlyToken. DANGEROUS: no validity checks!!"
Base.:&(f::F, andValue::T) where {F<:FlyToken,T<:Unsigned} = reinterpret(F,u64(f) & andValue)

"add an offset to a FlyToken (error if isdirect(f)). DANGEROUS: no validity or overflow checks!!"
function Base.:+(f::F, addValue::UInt32) where {F<:FlyToken} 
    isdirect(f) && error("cannot add offset to DirectFly")
    reinterpret(F,u64(f) + addValue)
end


## interpretation


Base.@propagate_inbounds function Base.iterate(s::AbstractToken, i::Int=firstindex(s))
    i > ncodeunits(s) && return nothing
    b = codeunit(s, i)
    u = UInt32(b) << 24
    # code from String
    #Base.between(b, 0x80, 0xf7) || return reinterpret(Char, u), i+1
    # does work for valid Utf8 but does not detect illegal code: bytes C0, C1, F5..F8 are all illegal,
    # according to wikipedia (referencing RFC3629), but are accepted here.
    # we can spare one comparison by this code:
    b >= 0x80 || return reinterpret(Char, u), i+1
    return Base.iterate_continued(s, i, u)
end

function Base.iterate_continued(s::AbstractToken, i::Int, u::UInt32)
    u < 0xc0000000 && (i += 1; @goto ret) # u<0x80000000 treated in iterate. all other u<0xc2000000 are illegal!! 
    n = ncodeunits(s)
    # first continuation byte
    (i += 1) > n && @goto ret
    @inbounds b = codeunit(s, i)
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 16
    # second continuation byte
    ((i += 1) > n) | (u < 0xe0000000) && @goto ret
    @inbounds b = codeunit(s, i)
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 8
    # third continuation byte
    ((i += 1) > n) | (u < 0xf0000000) && @goto ret
    @inbounds b = codeunit(s, i)
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b); i += 1
@label ret
    return reinterpret(Char, u), i
end




## Serialization of DirectFly

function Base.write(io::IO, t::DirectFly)
    write(io,Packed31(t))
    for i in 1:t.len
        write(io, @inbounds codeunit(t,i))
    end
end

  
"read contents of a DirectFly for already given category and size"
function Base.read(io::IO, cat_size::Packed31,::Type{DirectFly})
    size = bits4_30(cat_size)
    t = DirectFly(cat_size)
    for i in 1:size
        t = unsafe_setcodeunit(t,i,read(io,UInt8))
    end
    t
end



"read complete DirectFly"
function Base.read(io::IO, ::Type{DirectFly})
    read(io, read(io,Packed31),DirectFly) # category and size
end



Base.@propagate_inbounds function Base.codeunit(t::DirectFly, index::Int)
    @boundscheck checkbounds(t,index)
    (255%UInt8) & (u64(t) >>> ((7-index)<<3))%UInt8
end



Base.@propagate_inbounds function byte(t::DirectFly, ofs::UInt32)
    @boundscheck checkbyteofs(ofs,t.len)
    (255%UInt8) & (u64(t) >>> ((6-index)<<3))%UInt8
end


function Base.codeunit(t::FlyToken, index::Int)
    isdirect(t) || error("code unit buffer not available in codeunit($t,$index)")
    codeunit(df(t),index)
end


function Base.show(io::IO,t::T) where T <: FlyToken
    print(io, string(typeof(t))[1] * Char(t.cat))
    if isdirect(t)
        Base.print_quoted(io, df(t))
    else
        print(io,'<',offset(t)%Int,',',t.len%Int,'>')
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


#=

endianness
Base.lastindex(s::FlyToken) = Int(s.size_content & 0xf)
Base.iterate(s::FlyToken, i::Integer) = iterate(String(s), i)
Base.iterate(s::FlyToken) = iterate(String(s))
Base.print(s::FlyToken) = print(String(s))
Base.display(s::FlyToken) = display(String(s))
Base.firstindex(::FlyToken) = 1
Base.collect(s::FlyToken) = getindex.(s, 1:lastindex(s))

==(s::FlyToken, b::String) = begin
    String(s)  == b
end
=#
