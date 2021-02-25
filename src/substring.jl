"""
substring(source)

converts to a SubString{String}, trying to use an internal
string buffer. Works without content copying on Utf8String and IOShared objects

substring is intended to be used with (offset,size,source) tuples as argument on
byte buffers, not the String-oriented (source,firstIndex,lastIndex) notation introduced in
julia Base for AbstractString sources.

    
However, in all cases, UTF8 validity is checked (that is built into SubString constructor).
Only use substring for Utf8 string processing!!

"""
function substring end


"an empty string buffer"
const EMPTYSTRING = ""

usize(s::Union{String,SubString{String}}) = ncodeunits(s)%UInt64


"SubString construction using offset and size in bytes, with check of UTF8 content validity. "
Base.@propagate_inbounds function Base.SubString{String}(ofs::UInt32, size::UInt64,s::String)
    @boundscheck checksize(ofs+size,ncodeunits(s))
    return @inbounds SubString(s,ofs+1,thisind(s,(ofs+size)%Int))
end


substring(s::AbstractString) = substring(s,firstindex(s),lastindex(s))

substring(s::String,first::Integer,last::Integer) = SubString(s,first,last)

"expensive default (converts to String!)"
Base.@propagate_inbounds substring(s::AbstractString,first::Integer,last::Integer) = substring(string(s[first:last]))

Base.@propagate_inbounds substring(s::AbstractString,first::Integer) = substring(s,first, lastind(s))

Base.@propagate_inbounds substring(offset::UInt32, size::UInt64, s::AbstractString) = SubString(offset,size,string(s))

Base.@propagate_inbounds substring(offset::UInt32, size::UInt64, s::String) = SubString(offset,size,s)


"universal fallback implementation in (offset,size)-notation"
substring(offset::UInt32, size::UInt64, s) = substring(offset, size, string(s))


Base.@propagate_inbounds function substring(offset::UInt32, size::UInt64, s::SubString{String})
    @boundscheck checksize(offset+size,usize(s)) # necessary to ensure substring is in bounds of s, not only of s.string
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



