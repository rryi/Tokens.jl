
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
    reinterpret(Nibble,v%UInt8)
end


"""
    nibble(x::T)

For types T which include a nibble field, define nibble(..)
to extract that field. 

Recommended for bitfield-like types,  which have one exactly one Nibble field.

"""
function nibble end


Base.convert(::Type{UInt8}, x::Nibble) = reinterpret(UInt8,x)

Base.convert(::Type{Nibble}, x::UInt8) = Nibble(x)

Base.UInt8(x::Nibble) = reinterpret(UInt8,x)

Base.UInt64(x::Nibble) = reinterpret(UInt8,x)%UInt64

Base.Int64(x::Nibble) = reinterpret(UInt8,x)%Int64

Base.promote_rule(::Type{Nibble}, ::Type{U}) where U<:Unsigned  = UInt64

Base.promote_type(::Type{Nibble}, ::Type{Nibble}) = UInt8

Base.promote_rule(::Type{Nibble}, ::Type{U}) where U<:Signed = Int64

function Base.show(io::IO, v::Nibble) 
    code = UInt8(v)
    if code>0x09
        code += 0x07
    end
    print(io, Char('0'+code))
end 
    
#Base.promote_rule(::Type{T}, ::Type{Nibble}) where T<:Enum  = T

#Base.promote_rule(::Type{Nibble}, ::Type{T}) where T<:Enum  = Nibble

# Base.convert(::Type{T}, x::Nibble) where T<:Enum = T(UInt8(x))

Base.convert(::Type{Nibble}, x::T) where T<:Enum = Nibble(x)

Nibble(v::T) where T<:Enum = Nibble(UInt8(v))

Base.promote_rule(::Type{Nibble}, ::Type{T}) where T<:Enum  = Nibble


