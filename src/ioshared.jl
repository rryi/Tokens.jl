# This file is derived from julia iobuffer.jl. License is MIT: https://julialang.org/license

## work with String as UInt8 buffer via I/O primitives and share with tokens/substrings ##

# Stateful string

const MaybeIO = Union{IO,Nothing}

"""
Similar to IObuffer, but can share content with SubString and AbstractToken instances.

Maintains two internal pointers, one for the read position, one for the write
(==append) position. Can be used as byte queue (intzerrmittent reads and writes).
Reading tokens does never allocate on heap - returned tokens simply share the
internal buffer, IOShare instance will "copy on write" to protect shared data.

IOShared can be used for buffered input or output, by attaching a data source
or data sink.

IOShared can act as a mutable string, supporting in place content changes.

"""
mutable struct IOShared{T <: MaybeIO} <: IO
    buffer :: String # PRIVATE!! memory with byte data.
    ptr :: Ptr{UInt8} # PRIVATE!! pointer to buffer.
    readofs :: UInt32 # read position offset / number of consumed bytes
    writeofs :: UInt32 # write (append) position offset / end of defined data
    shared :: UInt32 # offset in buffer behind last shared byte (0: nothing shared)
    limit ::  UInt32 # total number of bytes in buffer
    preferredlimit ::  UInt32 # limit in case of reallocation due to sharing
    flushable :: Bool # true: io is target for flush. false: io is source for fill
    io::T # nothing or if flushable: target for flush else: source for fill
    function IOShared{T}(limit::UInt32,flushable::Bool,io::T) where T <: MaybeIO
        z = zero(UInt32)
        s = _string_n(limit)
        new(s,pointer(s),z,z,z,limit,limit,flushable,io)
    end
    function IOShared{Nothing}(offset::UInt32, size::UInt64, s::String)
        writeofs = offset+size
        limit = UInt32(ncodeunits(s)) # check on overflow necessary: cannot handle too long strings
        @boundscheck check_ofs_size(offset,size,limit)
        new(s,pointer(s),offset,writeofs,limit,limit,size%UInt32,false,nothing)
    end
end

IOShared(s::SubString{String}) = IOShared{Nothing}(UInt32(s.offset),s.ncodeunits%UInt64,s.string)

IOShared(s::String) = IOShared{Nothing}(zero(UInt32),(ncodeunits(s)%UInt64),s)

IOShared(t::BufferToken) = IOShared{Nothing}(offset(t.tiny),usize(t.tiny),t.buffer)

IOShared(t::Token) = IOShared(BufferToken(t))

IOShared(io::IOShared{Nothing}) = IOShared{Nothing}(io.readofs,(io.writeofs-io.readofs)%UInt64,share(io,io.limit))


## internal API

#"offset boundscheck, including "
#Base.checkbounds(::Type{Bool}, io::IOShared, ofs::UInt32) = io.readofs <= ofs <= io.writeofs

"""
return a readonly buffer reference, share bytes are guaranteed not to change.

Is regarded a private function. To obtain the text content of an IOShared,
use read operations. BufferToken()
"""
function share(io::IOShared,share::UInt32)
    if (io.shared<share)
        io.shared = share
    end
    io.buffer
end


"share the whole internal buffer. ATTENTION: may contain uninitialized data"
share(io::IOShared) = share(io,io.limit)


"currently loaded content size in bytes. Changes e.g. on fill, flush, read, write"
usize(io::IOShared) = (io.writeofs-io.readofs) % UInt64

"""
move memory within a String buffer.

Is named unsafe, because changes in a String instance are unsafe if
the string is referenced elsewere, and because no bounds checks are performed.

Method is regarded private and intended to be used within IOShared, only.
"""
function unsafe_move!(io::IOShared, dest::UInt32, src::UInt32, count::UInt)
    if count>0
        GC.@preserve io # for safety. might be unnecessary
        ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
              io.ptr+dest, io.ptr+src, count)
    end
    nothing
end



"""
if io.flushable, write all current content to data sink.

content is removed from this.
no buffer reorganization, but flushing can increase
re-use of an unshared buffer
"""
function Base.flush(io::IOShared{T} where T<:IO )
    if io.flushable && usize(io) > 0
        write(io.io,io.readofs,usize(io), io.buffer)
        io.readofs = io.writeofs
    end
    nothing
end


"""
allocate a new buffer and preserve content, inserting insert bytes at offset ofs.

Method tries to flush! first - this might free up enough memory to that less
data is copied and free memory is increased.

io is changed, adjusted ofs is returned

ofs precondition: io.readOfs <= ofs <= io.writeOfs

"""
function realloc!(io::IOShared, ofs::UInt32, insert::Int) ::UInt32
    @boundscheck io.readofs <= ofs && ofs <= io.writeofs || boundserror(io,ofs)
    flush(io)

    limit = io.preferredlimit
    while usize(io)+insert > limit
        # enlarge
        limit <<= 1
    end

    if io.io isa Nothing

    end
    ofs = flush!(io)
end




"""
ensure that io.buffer is writeable starting at ofs, and resize current content at ofs.

If resize is negative, deletion is restricted to the current size.
If io.shared>ofs, or if io.limit < ofs+resize, a new buffer is allocated.

Buffer allocation may change offsets, i.e. io.readofs, io.writeofs and ofs.
Instance variables are adjusted, the adjusted value of ofs is returned by modify.

Return adjusted ofs
"""
function modify!(io::IOShared, ofs::UInt32, resize::Int)
    @boundscheck checkbounds(io,ofs)
    if resize<0
        # correct resize if it would delete beyond end of content
        minresize = Int(ofs)-io.writeofs
        if (minresize > resize)
            resize = minresize
        end
        if io.shared>ofs
            # we need reallocation. Rare case, simple solution
            ofs = realloc!(io,ofs,0)
        end
        unsafe_move!(io,ofs,ofs-resize,io.writeofs+resize)
    else
        if io.shared>ofs || ofs+resize > io.limit
            # we need reallocation
            ofs = realloc(io,ofs,resize)
        else
            unsafe_move!(io,ofs+resize,ofs,io.writeofs-ofs)
        end
    end
    ofs
end


function ensure_writeable(io::IOShared, count::UInt64)
end




#=

# no overlap handling
ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
      dest, src, n)

# overlap handling

function unsafe_copyto!(dest::Ptr{T}, src::Ptr{T}, n) where T
    # Do not use this to copy data between pointer arrays.
    # It can't be made safe no matter how carefully you checked.
    ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
          dest, src, n*sizeof(T))
    return dest
end
=#



## Base API
##

Base.copy(b::IOShared{Nothing}) = IOShared(b)



function Base.show(io::IO, b::IOShared)
    print(io,   "IOShared(  readofs=",b.readofs,
                ", writeofs=", b.writeofs,
                ", shared=", b.shared,
                ", limit=", b.limit,
                ", '")
    len = io.writeofs-io.readofs
    if len<=11
        print(io,SubString(buffer,readofs+1,readofs+len))
    else
        print(io,SubString(buffer,readofs+1,readofs+5)),
        '~',SubString(buffer,size-4,size))
    end
    print(io,"')")
end


function Base.skip(io:IOShared, delta::Integer)
    d = UInt32(delta) #  checks negative
    if io.writeofs-io.readofs > d
        fill(d)
        if io.writeofs-io.readofs > d
            d = io.writeofs-io.readofs
        end
    end
    io.readofs += d
    return io # same return convention as skip(IOBuffer,..)
end


"optimized: no string allocation"
function Base.read(io::IO, ::Type{Token})
    cat,size = _readtokenheader(io)
    if size <= MAX_DIRECT_SIZE
        t = ht(_readdirecttoken(io,DirectToken(cat,size)))
        @inbounds Token(t,EMPTYSTRING)
    else
        @inbounds Token(ht(FlyToken(cat,io.readofs,size)), io.buffer)
        io.readofs += size
        share(io,io.readofs)
    end
end


function Base.read(io::IO, ::Type{BufferToken})
    cat,size = _readtokenheader(io)
    @inbounds BufferToken( FlyToken(cat,size), read(io,size,String))
end

"optimized: no string allocation"
function Base.read(io::IOShared, ::Type{Token})
    tt = read(io,HybridToken)
    if isdirect(tt)
        ss = EMPTYSTRING
    else
        # reference and skip string data in IOShared instance
        ofs = io.readofs
        size = usize(tt)
        tt = ht((u64(tt)& ~OFFSET_BITS) | ofs)
        ss = io.buffer
        io.readofs += size
        ss = share(io,io.readofs)
    end
    @inbounds Token(tt,ss)
end

function Base.read(io::IOShared, ::Type{BufferToken})
    t = read(io,Token)
    tt = t.tiny
    if isdirect(tt)
        # TODO can we reference bytes in io instance in this case??
        # possibly dependent on endianness...
        BufferToken(dt(tt))
    else
        # reference buffer
        tt = ft(u64(tt)& ~OFFSET_BITS)
        size = usize(tt)
        tt = ht((u64(tt)& ~OFFSET_BITS) | ofs)
        ss = io.buffer
        io.readofs += size
        @inbounds BufferToken(ft(tt), t.ss)
    end
end



function Base.unsafe_read(from::IOShared, p::Ptr{UInt8}, nb::UInt)
    #from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    ensure_readable(nb)
    b = from.buffer
    GC.@preserve b unsafe_copyto!(p, pointer(b, from.readofs), nb)
    from.readofs += nb
    nothing
end


function read(from::IOShared, ::UInt8)
    ensure_readable(1)
    ptr
    GC.@preserve b =
end

function read(from::IOShared, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64},Type{Int128},Type{UInt128},Type{Float16},Type{Float32},Type{Float64}})
    nb = sizeof(T)
    ensure_readable(nb)
    b = from.buffer
    GC.@preserve b begin
        ptr::Ptr{T} = pointer(b, from.readofs)
        x = unsafe_load(ptr)
    end
    from.readofs += nb
    return x
end

function read_sub(from::GenericIOBuffer, a::AbstractArray{T}, offs, nel) where T
    @assert !has_offset_axes(a)
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    if offs+nel-1 > length(a) || offs < 1 || nel < 0
        throw(BoundsError())
    end
    if isbitstype(T) && isa(a,Array)
        nb = Uint(nel * sizeof(T))
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
    ptr = from.readofs
    size = from.writeofs
    if ptr > size
        throw(EOFError())
    end
    @inbounds byte = codeunit(from.buffer,ptr+1)
    from.readofs = ptr + 1
    return byte
end

function peek(from::GenericIOBuffer)
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    if from.readofs > from.writeofs
        throw(EOFError())
    end
    return from.data[from.readofs]
end

read(from::GenericIOBuffer, ::Type{Ptr{T}}) where {T} = convert(Ptr{T}, read(from, UInt))

isreadable(io::GenericIOBuffer) = io.readable
iswritable(io::GenericIOBuffer) = io.writable

# TODO: GenericIOBuffer is not iterable, so doesn't really have a length.
# This should maybe be sizeof() instead.
#length(io::GenericIOBuffer) = (io.seekable ? io.writeofs : bytesavailable(io))
bytesavailable(io::GenericIOBuffer) = io.writeofs - io.readofs + 1
position(io::GenericIOBuffer) = io.readofs-1

function skip(io::GenericIOBuffer, n::Integer)
    seekto = io.readofs + n
    n < 0 && return seek(io, seekto-1) # Does error checking
    io.readofs = min(seekto, io.writeofs+1)
    return io
end

function seek(io::GenericIOBuffer, n::Integer)
    if !io.seekable
        ismarked(io) || throw(ArgumentError("seek failed, IOBuffer is not seekable and is not marked"))
        n == io.mark || throw(ArgumentError("seek failed, IOBuffer is not seekable and n != mark"))
    end
    # TODO: REPL.jl relies on the fact that this does not throw (by seeking past the beginning or end
    #       of an GenericIOBuffer), so that would need to be fixed in order to throw an error here
    #(n < 0 || n > io.writeofs) && throw(ArgumentError("Attempted to seek outside IOBuffer boundaries."))
    #io.readofs = n+1
    io.readofs = max(min(n+1, io.writeofs+1), 1)
    return io
end

function seekend(io::GenericIOBuffer)
    io.readofs = io.writeofs+1
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
    io.data[io.writeofs+1:n] .= 0
    io.writeofs = n
    io.readofs = min(io.readofs, n+1)
    ismarked(io) && io.mark > n && unmark(io)
    return io
end

function compact(io::GenericIOBuffer)
    io.writable || throw(ArgumentError("compact failed, IOBuffer is not writeable"))
    io.seekable && throw(ArgumentError("compact failed, IOBuffer is seekable"))
    local ptr::Int, bytes_to_move::Int
    if ismarked(io) && io.mark < io.readofs
        if io.mark == 0 return end
        ptr = io.mark
        bytes_to_move = bytesavailable(io) + (io.readofs-io.mark)
    else
        ptr = io.readofs
        bytes_to_move = bytesavailable(io)
    end
    copyto!(io.data, 1, io.data, ptr, bytes_to_move)
    io.writeofs -= ptr - 1
    io.readofs -= ptr - 1
    io.mark -= ptr - 1
    return io
end

@inline ensureroom(io::GenericIOBuffer, nshort::Int) = ensureroom(io, UInt(nshort))
@inline function ensureroom(io::GenericIOBuffer, nshort::UInt)
    io.writable || throw(ArgumentError("ensureroom failed, IOBuffer is not writeable"))
    if !io.seekable
        nshort >= 0 || throw(ArgumentError("ensureroom failed, requested number of bytes must be ≥ 0, got $nshort"))
        if !ismarked(io) && io.readofs > 1 && io.writeofs <= io.readofs - 1
            io.readofs = 1
            io.writeofs = 0
        else
            datastart = ismarked(io) ? io.mark : io.readofs
            if (io.writeofs+nshort > io.limit) ||
                (datastart > 4096 && datastart > io.writeofs - io.readofs) ||
                (datastart > 262144)
                # apply somewhat arbitrary heuristics to decide when to destroy
                # old, read data to make more room for new data
                compact(io)
            end
        end
    end
    n = min(nshort + (io.append ? io.writeofs : io.readofs-1), io.limit)
    if n > length(io.data)
        resize!(io.data, n)
    end
    return io
end

eof(io::GenericIOBuffer) = (io.readofs-1 == io.writeofs)

@noinline function close(io::GenericIOBuffer{T}) where T
    io.readable = false
    io.writable = false
    io.seekable = false
    io.writeofs = 0
    io.limit = 0
    io.readofs = 1
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
        nbytes = io.writeofs
        data = copyto!(StringVector(nbytes), 1, io.data, 1, nbytes)
    else
        nbytes = bytesavailable(io)
        data = read!(io,StringVector(nbytes))
    end
    if io.writable
        io.readofs = 1
        io.writeofs = 0
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
        resize!(data,io.writeofs)
    else
        nbytes = bytesavailable(io)
        a = StringVector(nbytes)
        data = read!(io, a)
    end
    if io.writable
        io.readofs = 1
        io.writeofs = 0
    end
    return data
end

function write(to::GenericIOBuffer, from::GenericIOBuffer)
    if to === from
        from.readofs = from.writeofs + 1
        return 0
    end
    written::Int = write_sub(to, from.data, from.readofs, bytesavailable(from))
    from.readofs += written
    return written
end

function unsafe_write(to::GenericIOBuffer, p::Ptr{UInt8}, nb::UInt)
    ensureroom(to, nb)
    ptr = (to.append ? to.writeofs+1 : to.readofs)
    written = Int(min(nb, length(to.data) - ptr + 1))
    towrite = written
    d = to.data
    while towrite > 0
        @inbounds d[ptr] = unsafe_load(p)
        ptr += 1
        p += 1
        towrite -= 1
    end
    to.writeofs = max(to.writeofs, ptr - 1)
    if !to.append
        to.readofs += written
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
    ptr = (to.append ? to.writeofs+1 : to.readofs)
    if ptr > to.limit
        return 0
    else
        to.data[ptr] = a
    end
    to.writeofs = max(to.writeofs, ptr)
    if !to.append
        to.readofs += 1
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
    p = pointer(buf.data, buf.readofs)
    q = GC.@preserve buf ccall(:memchr,Ptr{UInt8},(Ptr{UInt8},Int32,Csize_t),p,delim,bytesavailable(buf))
    return q != C_NULL
end

function occursin(delim::UInt8, buf::GenericIOBuffer)
    data = buf.data
    for i = buf.readofs:buf.writeofs
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
    for i = io.readofs : io.writeofs
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
    io.readofs += nread
    if lb != nout
        resize!(A, nout)
    end
    A
end

# copy-size crc32c of IOBuffer:
function _crc32c(io::IOBuffer, nb::Integer, crc::UInt32=0x00000000)
    nb < 0 && throw(ArgumentError("number of bytes to checksum must be ≥ 0"))
    io.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    n = min(nb, bytesavailable(io))
    n == 0 && return crc
    crc = GC.@preserve io unsafe_crc32c(pointer(io.data, io.writeofs), n, crc)
    io.writeofs += n
    return crc
end
_crc32c(io::IOBuffer, crc::UInt32=0x00000000) = _crc32c(io, bytesavailable(io), crc)


function Base.checkbounds(io:IOShared, i:Integer)
    if (i<=0) || (i > io.writeofs)
        throw BoundsError(io,i)
    end
    nothing
end
