
# Nibble is a half byte (4 bits). 
# This file defines the Nibble type and related stuff 

"""
Nibble is basically an unsigned integer stored in a half byte, giving the data range 0..15

It is defined as an explicit type to allow for clear method signatures.
"""
#primitive type Nibble <: Number 8 end
primitive type Nibble <: Unsigned 8 end

function Nibble(v::Integer) ::Nibble
    @boundscheck checklimit(v,0x0F)
    #@boundscheck v <= 0x0F || error("Not in Nibble value range: $v")
    reinterpret(Nibble,v%UInt8)
end

Base.convert(::Type{UInt8}, x::Nibble) = reinterpret(UInt8,x)

Base.convert(::Type{Nibble}, x::UInt8) = Nibble(x)

Base.UInt8(x::Nibble) = reinterpret(UInt8,x)

Base.UInt64(x::Nibble) = reinterpret(UInt8,x)%UInt64

Base.Int64(x::Nibble) = reinterpret(UInt8,x)%Int64

Base.promote_rule(::Type{Nibble}, ::Type{U}) where U<:Unsigned  = UInt64

Base.promote_type(::Type{Nibble}, ::Type{Nibble}) = UInt8

Base.promote_rule(::Type{Nibble}, ::Type{U}) where U<:Signed = Int64

Base.show(io::IO, v::Nibble) = show(io,UInt8(v))


