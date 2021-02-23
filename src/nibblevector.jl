"""
NibbleVector is an immutable Vector of Nibbles (4 bit integers, values 0..15) of length 16, implemented as bitfield in 64 bits.
"""
primitive type NibbleVector <: AbstractVector{UInt8} 64 end

# TODO: 


