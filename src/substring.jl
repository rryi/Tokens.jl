


"These string types have methods operating with Utf8 code units"
const Utf8String = Union{String,SubString{String},AbstractToken}

"an empty string buffer"
const EMPTYSTRING = ""


"fast SubString construction using offset and size, without validation of UTF8 content validity"
Base.@propagate_inbounds function Base.SubString{String}(ofs::UInt32, size::UInt64,s::String)
    @boundscheck checksize(ofs+size,ncodeunits(s))
    return @inbounds SubString(s,ofs+1,(ofs+size)%Int)
end


"""
    substring(source)

converts to a SubString{String}, trying to use an internal
string buffer. Works without content copying on Utf8String and IOBuffer objects

"""
function substring end


substring(s::String,first::Integer,last::Integer) = SubString(s,first,last)

Base.@propagate_inbounds substring(s::AbstractString,first::Integer,last::Integer) = substring((first-1)%UInt32,(last-(first-1))%UInt64,s)

Base.@propagate_inbounds substring(offset::UInt32, size::UInt64, s::AbstractString) = SubString(offset,size,string(s))

Base.@propagate_inbounds substring(offset::UInt32, size::UInt64, s::String) = SubString(offset,size,s)

Base.@propagate_inbounds function substring(offset::UInt32, size::UInt64, s::SubString{String})
    @boundscheck checksize(offset+size,ncodeunits(s)) # necessary to ensure substring is in bounds of s, not only of s.string
    @inbounds return SubString(offset+s.offset,size,s.string)
end


## enhancements of write and read - with prefix t to avoid type piracy


"""
optimized 'substring writing' of code units from a string buffer segment.

Not overloading Base.write because of inconsistency with generic multi-parameter-write 
"""
function sswrite(io::IO, ofs::UInt32, size::UInt64, s::String)
    @boundscheck inbounds(s,ofs+size)
    GC.@preserve s unsafe_write(io, pointer(s)+ofs, size) # maybe fails on 32 bit julia implementations? size is not type UInt
end


"""
optimized reading of bytes (aka code units) into a string buffer
"""
function Base.read(io::IO, size::UInt64, ::Type{String})
    ss = _string_n(size)
    GC.@preserve ss unsafe_read(io, pointer(ss), UInt(size))
end



