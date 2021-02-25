
## serialization helper type: Packed31 ##


"""
Helper type to read/write 31 bits in 1/2/5 bytes.

Why 31 bits and not 32?

 * encoding is "dense", no redundancy
 * category+length in FlyToken have exactly 31 bits

"""
primitive type Packed31 32 end

function Packed31(v::Union{UInt32,Int32})
    reinterpret(Packed31,v)
end

Base.UInt32(v::Packed31) = reinterpret(UInt32,v)


Base.show(io::IO, v::Packed31) = show(io,UInt32(v))



"extracts the lowest 4 bits from  an unsigned value as an UInt8"
function bits0_3(v)
    (v%UInt8) & 0x0f  
end
bits0_3(v::Packed31) = bits0_3(UInt32(v))

nibble(v::Packed31) = bits0_3(v)

"right shift by a nibble (4 bits)"
bits4_30(v::Packed31) = UInt32(v) >> 4

function Packed31(bits0_3::UInt8, bits4_30::UInt32)
    @boundscheck checklimit(bits0_3,15) 
    @boundscheck checklimit(bits4_30,(1<<27)-1) 
    Packed31((bits4_30<<4) + bits0_3)
end


"""
    read(io::IO,::Packed31)

read 31 bits in a variable length encoding format of 1/2/5 bytes.
"""
function Base.read(io::IO,::Type{Packed31})
    v = read(io,UInt8) %UInt32
    if v <= 127 
        return Packed31(v)
    end
    v = ((v-128) <<8 ) | (read(io,UInt8)%UInt32)
    if v <= 127
        # 5 byte encoding
        v = (value << 24) |  ((read(io,UInt8)%UInt32)<<16)  |  ((read(io,UInt8)%UInt32)<<8)  |  ((read(io,UInt8)%UInt32))
    end
    return Packed31(v)
end


"""
    write(io::IO, v::Packed31)

write 31 bits in variable length encoding in an platform independent format. 
0..127 are encoded in 1 byte, 128..32767 in 2 bytes, all other need 5 bytes.

This encoding is recommended for serialization of nonnegative Int32 
values if encoded size matters and if small values dominate, 
e.g. 50% are below 32768 or 25% are below 128.

"""
function Base.write(io::IO, v::Packed31)
    u = UInt32(v)
    if u<=127
        write(io,u%UInt8)
        return nothing
    end
    if  u <= 32767
        write(io,hton(u%UInt16+0x8000))
        return nothing
    end
    write(io,0x80)
    write(io,hton(u))
    nothing
end

