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

(a) method does not access code units, like *ncodeunits*, *category*
or *isdirect*.

(b) method needs code unit access, and knows the buffer from its context.


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

The advantage over FlyToken is less memory consumption, and faster
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


####################################################################
### basic helpers ##################################################
####################################################################





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

offset(t::FlyToken) = convert(UInt32,u64(t) & OFFSET_BITS)

offset(t::DirectToken) = UInt32(0)

offset(t::HybridToken) = isdirect(t) ? offset(dt(t)) : offset(ft(t))




####################################################################
### constructors  ##################################################
####################################################################



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


     ####################################################################
     ### Base operators and functions overloading #######################
     ####################################################################



Base.sizeof(t::DirectToken) = (u64(t)&DIRECT_SIZEBITS)>>56


function Base.sizeof(t::FlyToken)
    if FLY_SPLIT_SIZE
        (u64(t)&DIRECT_SIZE_BITS) >>56 | (u64(t)&SPLIT_SIZE_BITS)>>29
    else
         (u64(t)&FLY_SIZE_BITS)>>32
    end
end


function Base.sizeof(t::HybridToken)
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
            sizeof(dt(t))
        else
            sizeof(ft(t))
        end
    end
end


function Base.codeunit(t::DirectToken, index::Integer)
    @boundscheck checkbounds(t,index)
    UInt8(UInt8(255) & (u64(t) >>> ((7-index)<<3)))
end


function Base.codeunit(t::TinyToken, index::Integer)
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

#########################################################
################ Token API methods ######################
#########################################################


category(t::TinyToken) = reinterpret(TCategory, UInt8((u64(t) >>>59)&15))

category(t::DirectToken) = reinterpret(TCategory, UInt8(u64(t) >>>59))

isdirect(t::TinyToken) = reinterpret(Int64,t)>=0

isdirect(t::DirectToken) = true

isdirect(t::FlyToken) = false


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


"throw an error if token references some buffer"
Base.@propagate_inbounds function checkappend(t::TinyToken,append)
    @boundscheck checktiny(t)
    @boundscheck ((t.bits>>56 & 7) + append <=7) ||
        throw(ErrorException("no &append bytes left in tinytoken: &t"))
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
