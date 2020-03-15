# This file is derived from julia iobuffer.jl. License is MIT: https://julialang.org/license

## work with String as UInt8 buffer via I/O primitives and share with tokens/substrings ##

# Stateful string

const MaybeIO = Union{IO,Nothing}

mutable struct IOShared{T} where T <: MaybeIO <: IO
    buffer :: String # PRIVATE!! memory with token/substring text data.
    skip :: UInt32 # read position offset: number of consumed bytes
    free :: UInt32 # size and append position offset
    shared :: UInt32 # last index in buffer shared with other objects (0 is valid)
    limit ::  UInt32 # total number of bytes in buffer
    preferredlimit ::  UInt32 # limit in case of reallocation due to sharing
    flushable :: Bool # true: io is target for flush. false: io is source for fill
    io::T # if not nothing, if flushable: target for flush else source for fill

    function IOShared(limit::Integer,flushable::Bool=false,io::MaybeIO=nothing)
        new(_string_n(limit),0,0,0,limit,limit,flushable,io)
    end
    function IOShared(io::IOShared)
        new(share!(io,io.free),io.skip,io.free,io.limit,io.limit,nothing,false)
    end
    function IOShared(s::String)
        size = UInt32(ncodeunits(s))
        new(s,0,size,size,size,size)
    end
    function IOShared(s::SubString{String})
        ofs = s.offset
        size = s.ncodeunits+ofs
        lim = ncodeunits(s.string)
        new(s.string,ofs,size,lim,lim,size)
    end
    function IOShared(t:BufferToken)
        ofs = offset(t.tiny)
        fre = ofs + sizeof(t.tiny)
        lim = UInt32(sizeof(t.buffer))
        new(t.buffer,ofs,fre,lim,sizeof(t.tiny))
    end
end

"""
return a readonly buffer reference, size bytes guaranteed not to change
"""
function share!(io::IOShared,size::UInt32)
    if (io.shared<size)
        io.shared = size
    end
    io.buffer
end


copy(b::IOshare) = IOShared(b)

function show(io::IO, b::IOShared)
    print(io,   "IOShared(  skip=",b.skip,
                ", free=", b.free,
                ", shared=", b.shared,
                ", limit=", b.limit,
                ", '")
    len = free-skip
    if len<=11
        print(io,SubString(buffer,skip+1,skip+len))
    else
        print(io,SubString(buffer,skip+1,skip+5)),
        '~',SubString(buffer,free-4,free))
    end
    print(io,"')")
end

#TODO copy aus string.jl
@inline function Base.codeunit(io:IOshared, i::Integer)
    @boundscheck checkbounds(io, i)
    GC.@preserve s unsafe_load(pointer(io.buffer, i))
end

function unsafe_read(from::GenericIOBuffer, p::Ptr{UInt8}, nb::UInt)
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    avail = bytesavailable(from)
    adv = min(avail, nb)
    GC.@preserve from unsafe_copyto!(p, pointer(from.data, from.skip), adv)
    from.skip += adv
    if nb > avail
        throw(EOFError())
    end
    nothing
end

function read(from::GenericIOBuffer, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64},Type{Int128},Type{UInt128},Type{Float16},Type{Float32},Type{Float64}})
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    avail = bytesavailable(from)
    nb = sizeof(T)
    if nb > avail
        throw(EOFError())
    end
    GC.@preserve from begin
        ptr::Ptr{T} = pointer(from.data, from.skip)
        x = unsafe_load(ptr)
    end
    from.skip += nb
    return x
end

function read_sub(from::GenericIOBuffer, a::AbstractArray{T}, offs, nel) where T
    @assert !has_offset_axes(a)
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    if offs+nel-1 > length(a) || offs < 1 || nel < 0
        throw(BoundsError())
    end
    if isbitstype(T) && isa(a,Array)
        nb = UInt(nel * sizeof(T))
        GC.@preserve a unsafe_read(from, pointer(a, offs), nb)
    else
        for i = offs:offs+nel-1
            a[i] = read(to, T)
        end
    end
    return a
end

@inline function read(from::IOShared, ::Type{UInt8})
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    skip = from.skip
    free = from.free
    if skip > free
        throw(EOFError())
    end
    @inbounds byte = codeunit(from.buffer,skip+1)
    from.skip = skip + 1
    return byte
end

function peek(from::GenericIOBuffer)
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    if from.skip > from.free
        throw(EOFError())
    end
    return from.data[from.skip]
end

read(from::GenericIOBuffer, ::Type{Ptr{T}}) where {T} = convert(Ptr{T}, read(from, UInt))

isreadable(io::GenericIOBuffer) = io.readable
iswritable(io::GenericIOBuffer) = io.writable

# TODO: GenericIOBuffer is not iterable, so doesn't really have a length.
# This should maybe be sizeof() instead.
#length(io::GenericIOBuffer) = (io.seekable ? io.free : bytesavailable(io))
bytesavailable(io::GenericIOBuffer) = io.free - io.skip + 1
position(io::GenericIOBuffer) = io.skip-1

function skip(io::GenericIOBuffer, n::Integer)
    seekto = io.skip + n
    n < 0 && return seek(io, seekto-1) # Does error checking
    io.skip = min(seekto, io.free+1)
    return io
end

function seek(io::GenericIOBuffer, n::Integer)
    if !io.seekable
        ismarked(io) || throw(ArgumentError("seek failed, IOBuffer is not seekable and is not marked"))
        n == io.mark || throw(ArgumentError("seek failed, IOBuffer is not seekable and n != mark"))
    end
    # TODO: REPL.jl relies on the fact that this does not throw (by seeking past the beginning or end
    #       of an GenericIOBuffer), so that would need to be fixed in order to throw an error here
    #(n < 0 || n > io.free) && throw(ArgumentError("Attempted to seek outside IOBuffer boundaries."))
    #io.skip = n+1
    io.skip = max(min(n+1, io.free+1), 1)
    return io
end

function seekend(io::GenericIOBuffer)
    io.skip = io.free+1
    return io
end

function truncate(io::GenericIOBuffer, n::Integer)
    io.writable || throw(ArgumentError("truncate failed, IOBuffer is not writeable"))
    io.seekable || throw(ArgumentError("truncate failed, IOBuffer is not seekable"))
    n < 0 && throw(ArgumentError("truncate failed, n bytes must be ≥ 0, got $n"))
    n > io.limit && throw(ArgumentError("truncate failed, $(n) bytes is exceeds IOBuffer limit $(io.limit)"))
    if n > length(io.data)
        resize!(io.data, n)
    end
    io.data[io.free+1:n] .= 0
    io.free = n
    io.skip = min(io.skip, n+1)
    ismarked(io) && io.mark > n && unmark(io)
    return io
end

function compact(io::GenericIOBuffer)
    io.writable || throw(ArgumentError("compact failed, IOBuffer is not writeable"))
    io.seekable && throw(ArgumentError("compact failed, IOBuffer is seekable"))
    local ptr::Int, bytes_to_move::Int
    if ismarked(io) && io.mark < io.skip
        if io.mark == 0 return end
        ptr = io.mark
        bytes_to_move = bytesavailable(io) + (io.skip-io.mark)
    else
        ptr = io.skip
        bytes_to_move = bytesavailable(io)
    end
    copyto!(io.data, 1, io.data, ptr, bytes_to_move)
    io.free -= ptr - 1
    io.skip -= ptr - 1
    io.mark -= ptr - 1
    return io
end

@inline ensureroom(io::GenericIOBuffer, nshort::Int) = ensureroom(io, UInt(nshort))
@inline function ensureroom(io::GenericIOBuffer, nshort::UInt)
    io.writable || throw(ArgumentError("ensureroom failed, IOBuffer is not writeable"))
    if !io.seekable
        nshort >= 0 || throw(ArgumentError("ensureroom failed, requested number of bytes must be ≥ 0, got $nshort"))
        if !ismarked(io) && io.skip > 1 && io.free <= io.skip - 1
            io.skip = 1
            io.free = 0
        else
            datastart = ismarked(io) ? io.mark : io.skip
            if (io.free+nshort > io.limit) ||
                (datastart > 4096 && datastart > io.free - io.skip) ||
                (datastart > 262144)
                # apply somewhat arbitrary heuristics to decide when to destroy
                # old, read data to make more room for new data
                compact(io)
            end
        end
    end
    n = min(nshort + (io.append ? io.free : io.skip-1), io.limit)
    if n > length(io.data)
        resize!(io.data, n)
    end
    return io
end

eof(io::GenericIOBuffer) = (io.skip-1 == io.free)

@noinline function close(io::GenericIOBuffer{T}) where T
    io.readable = false
    io.writable = false
    io.seekable = false
    io.free = 0
    io.limit = 0
    io.skip = 1
    io.mark = -1
    if io.writable
        resize!(io.data, 0)
    end
    nothing
end

isopen(io::GenericIOBuffer) = io.readable || io.writable || io.seekable || bytesavailable(io) > 0

"""
    take!(b::IOBuffer)

Obtain the contents of an `IOBuffer` as an array, without copying. Afterwards, the
`IOBuffer` is reset to its initial state.

# Examples
```jldoctest
julia> io = IOBuffer();

julia> write(io, "JuliaLang is a GitHub organization.", " It has many members.")
56

julia> String(take!(io))
"JuliaLang is a GitHub organization. It has many members."
```
"""
function take!(io::GenericIOBuffer)
    ismarked(io) && unmark(io)
    if io.seekable
        nbytes = io.free
        data = copyto!(StringVector(nbytes), 1, io.data, 1, nbytes)
    else
        nbytes = bytesavailable(io)
        data = read!(io,StringVector(nbytes))
    end
    if io.writable
        io.skip = 1
        io.free = 0
    end
    return data
end
function take!(io::IOBuffer)
    ismarked(io) && unmark(io)
    if io.seekable
        data = io.data
        if io.writable
            limit = (io.limit == typemax(Int) ? 0 : min(length(io.data),io.limit))
            io.data = StringVector(limit)
        else
            data = copy(data)
        end
        resize!(data,io.free)
    else
        nbytes = bytesavailable(io)
        a = StringVector(nbytes)
        data = read!(io, a)
    end
    if io.writable
        io.skip = 1
        io.free = 0
    end
    return data
end

function write(to::GenericIOBuffer, from::GenericIOBuffer)
    if to === from
        from.skip = from.free + 1
        return 0
    end
    written::Int = write_sub(to, from.data, from.skip, bytesavailable(from))
    from.skip += written
    return written
end

function unsafe_write(to::GenericIOBuffer, p::Ptr{UInt8}, nb::UInt)
    ensureroom(to, nb)
    ptr = (to.append ? to.free+1 : to.skip)
    written = Int(min(nb, length(to.data) - ptr + 1))
    towrite = written
    d = to.data
    while towrite > 0
        @inbounds d[ptr] = unsafe_load(p)
        ptr += 1
        p += 1
        towrite -= 1
    end
    to.free = max(to.free, ptr - 1)
    if !to.append
        to.skip += written
    end
    return written
end

function write_sub(to::GenericIOBuffer, a::AbstractArray{UInt8}, offs, nel)
    @assert !has_offset_axes(a)
    if offs+nel-1 > length(a) || offs < 1 || nel < 0
        throw(BoundsError())
    end
    GC.@preserve a unsafe_write(to, pointer(a, offs), UInt(nel))
end

@inline function write(to::GenericIOBuffer, a::UInt8)
    ensureroom(to, UInt(1))
    ptr = (to.append ? to.free+1 : to.skip)
    if ptr > to.limit
        return 0
    else
        to.data[ptr] = a
    end
    to.free = max(to.free, ptr)
    if !to.append
        to.skip += 1
    end
    return sizeof(UInt8)
end

readbytes!(io::GenericIOBuffer, b::Array{UInt8}, nb=length(b)) = readbytes!(io, b, Int(nb))
function readbytes!(io::GenericIOBuffer, b::Array{UInt8}, nb::Int)
    nr = min(nb, bytesavailable(io))
    if length(b) < nr
        resize!(b, nr)
    end
    read_sub(io, b, 1, nr)
    return nr
end
read(io::GenericIOBuffer) = read!(io,StringVector(bytesavailable(io)))
readavailable(io::GenericIOBuffer) = read(io)
read(io::GenericIOBuffer, nb::Integer) = read!(io,StringVector(min(nb, bytesavailable(io))))

function occursin(delim::UInt8, buf::IOBuffer)
    p = pointer(buf.data, buf.skip)
    q = GC.@preserve buf ccall(:memchr,Ptr{UInt8},(Ptr{UInt8},Int32,Csize_t),p,delim,bytesavailable(buf))
    return q != C_NULL
end

function occursin(delim::UInt8, buf::GenericIOBuffer)
    data = buf.data
    for i = buf.skip:buf.free
        @inbounds b = data[i]
        b == delim && return true
    end
    return false
end

function readuntil(io::GenericIOBuffer, delim::UInt8; keep::Bool=false)
    lb = 70
    A = StringVector(lb)
    nread = 0
    nout = 0
    data = io.data
    for i = io.skip : io.free
        @inbounds b = data[i]
        nread += 1
        if keep || b != delim
            nout += 1
            if nout > lb
                lb = nout*2
                resize!(A, lb)
            end
            @inbounds A[nout] = b
        end
        if b == delim
            break
        end
    end
    io.skip += nread
    if lb != nout
        resize!(A, nout)
    end
    A
end

# copy-free crc32c of IOBuffer:
function _crc32c(io::IOBuffer, nb::Integer, crc::UInt32=0x00000000)
    nb < 0 && throw(ArgumentError("number of bytes to checksum must be ≥ 0"))
    io.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    n = min(nb, bytesavailable(io))
    n == 0 && return crc
    crc = GC.@preserve io unsafe_crc32c(pointer(io.data, io.free), n, crc)
    io.free += n
    return crc
end
_crc32c(io::IOBuffer, crc::UInt32=0x00000000) = _crc32c(io, bytesavailable(io), crc)


function Base.checkbounds(io:IOshared, i:Integer)
    if (i<=0) || (i > io.free)
        throw BoundsError(io,i)
    end
    nothing
end
