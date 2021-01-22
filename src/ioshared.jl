# This file is derived from julia iobuffer.jl. License is MIT: https://julialang.org/license

## work with String as UInt8 buffer via I/O primitives and share with tokens/substrings ##

# Stateful string

const MaybeIO = Union{IO,Nothing}

"""
IO buffer which can share content with SubString and AbstractToken instances.

Reading of tokens or substrings can be done without heap allocations - 
such returned items simply share the internal buffer, IOShare instance 
will "copy on write" to protect shared data.

IOShared comes in three flavours:

 * IOShared{Nothing}: closely resembles IOBuffer with some additional 
 text processing methods giving a "mutable string" behavior.

 * IOShared{T<:IO}, flushable: buffered output stream.
 writeable but not readable, all data written finally go to a data sink
 attached by constructor.

 * IOShared{T<:IO}, not flushable: buffered input stream.
 readable but not writeable, internal buffer is filled from a data source
 attached by constructor.


Maintains two internal offsets, marking the currently valid part of the internal buffer.
One is the current read position, the other the current write
(==append) position. IOShared{Nothing} can be used as byte queue (intermittent reads and writes).



"""
mutable struct IOShared{T <: MaybeIO} <: IO
    buffer :: String # PRIVATE!! memory with byte data.
    ptr :: Ptr{UInt8} # PRIVATE!! pointer to buffer.
    readofs :: UInt32 # read position offset / number of consumed bytes
    writeofs :: UInt32 # write (append) position offset / end of defined data
    shared :: UInt32 # offset in buffer behind last shared byte (0: nothing shared)
    limit ::  UInt32 # total number of bytes in buffer
    preferredlimit ::  UInt32 # limit in case of reallocation due to sharing
    flushable :: Bool # true: io is target for flush. false: io is source for fillup
    io::T # nothing or if flushable: target for flush else: source for fillup
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

IOShared(limit::UInt32) = IOShared(limit,false,nothing)

IOShared(s::SubString{String}) = IOShared{Nothing}(UInt32(s.offset),s.ncodeunits%UInt64,s.string)

IOShared(s::String) = IOShared{Nothing}(zero(UInt32),(ncodeunits(s)%UInt64),s)

IOShared(t::BToken) = IOShared{Nothing}(offset(t.tiny),usize(t.tiny),t.buffer)

IOShared(t::Token) = IOShared(BToken(t))

IOShared(io::IOShared{Nothing}) = IOShared{Nothing}(io.readofs,(io.writeofs-io.readofs)%UInt64,share(io,io.limit))


## internal API

#"offset boundscheck, including "
#Base.checkbounds(::Type{Bool}, io::IOShared, ofs::UInt32) = io.readofs <= ofs <= io.writeofs

"""
return a readonly buffer reference, share bytes are guaranteed not to change.

Is regarded a private function. To obtain the text content of an IOShared,
use read operations. BToken()
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
move memory within a IOShared buffer: insert/delete at ofs

Method is regarded private and intended to be used within IOShared, only.

ofs is internal offset into io-s buffer.
PRECOND: limit large enough, no share at ofs, no delete beyond io.writeofs
 ofs <= io.writeofs,
 if resize<0: ofs-resize <= io.writeofs

"""
function _resize(io::IOShared, ofs::UInt32, resize::Int)
    @boundscheck begin
        (io.shared <= ofs # never chance shared contents
        && ofs <= io.writeofs # not beyond end of content
        && ofs <= io.writeofs + resize  # no delete beyond content (resize<0)
        && io.writeofs+resize <= io.limit # enough space (relevant if resize>0)
        ) || boundserror(io,ofs, resize)
    end
    s = io.buffer
    GC.@preserve s begin
        if resize>=0
            ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                  io.ptr+ofs+resize, io.ptr+ofs, io.writeofs-ofs)
        else
            ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                  io.ptr+ofs, io.ptr+ofs-resize, io.writeofs-ofs+resize)
        end
    end
    nothing
end


"""
allocate a new buffer and preserve content, resizing at offset ofs.

Method tries to flush first, to reduce allocation as well as
the amount of bytes to copy

io is changed, adjusted ofs is returned

ofs precondition: io.readOfs <= ofs <= io.writeOfs

"""
function realloc!(io::IOShared, ofs::UInt32, resize::Int) ::UInt32
    flush(io)
    size = usize(io)%Int + resize
    @boundscheck begin
        (io.readofs <= ofs 
        && ofs <= io.writeofs
        && size >= 0 && size <= typemax(UInt32)
        && (ofs-resize <= io.writeofs)
        ) || boundserror(io,ofs,resize)
    end
    # new limit: several considerations.
    # (1) at least, it must be >= size
    # (2) should not be smaller then preferredlimit
    # (3) ensure that write(UInt8) is O(1) ==> an O(1) realloc seldom a constantly growing buffer (by small appends)
    limit = io.preferredlimit
    while size > limit
        # enlarge
        limit <<= 1 # ensures consideration (3): double buffer sizes
        limit == 0 && (limit = typemax(UInt32)) # handles overflow
    end
    buf = _string_n(limit)
    ptr = pointer(buf)
    count = (ofs - io.readofs) %UInt
    GC.@preserve buffer buf # for safety. might be unnecessary
    begin
        ## copy first part
        if count>0
            ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                  ptr, io.ptr+io.readofs, count)
        end
        # copy 2nd part: ofs..io.writeofs
        if resize<0
            count2 =  (io.writeofs-ofs+resize) %UInt
            if count2>0
                ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                    ptr+count, io.ptr+ofs-resize, count2)
            end
        else
            count2 =  (io.writeofs-ofs) %UInt
            if count2>0
                ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                    ptr+count+resize, io.ptr+io.ofs, count2)
            end
        end
    end
    ofs -= io.readofs
    io.readofs = 0
    io.shared = 0
    io.writeofs = size %UInt32
    io.limit = limit
    io.buffer = buf
    io.ptr = ptr
    return ofs
end


"""
ensure that io.buffer is writeable starting at ofs, and resize current content at ofs.

If resize is negative, deletion is restricted to the current content.
If io.shared>ofs, or if io.limit < ofs+resize, a new buffer is allocated.

Buffer allocation may change offsets, i.e. io.readofs, io.writeofs and ofs.
Instance variables are adjusted, the adjusted value of ofs is returned by modify.

Return adjusted ofs
"""
function modify!(io::IOShared, ofs::UInt32, resize::Int)
    if io.shared<=ofs
        # good chances for a noop
        if io.writeofs+resize<=io.limit
            # insert/delete
            if ofs==io.writeofs && resize >=0
                # normal append: expected to be the most frequent case
                return ofs
            end
            _resize(io,ofs,resize)
            return ofs
        end
        # not enough space at end - can we reuse space at beginning?
        flush(io)
        delta::Int = io.shared - io.readofs # bytes deleteable at shared if <0
        if io.writeofs+resize+delta <= io.limit
            # resizing is enough
            _resize(io,io.shared,delta) # remove free space at beginning
            ofs += delta
            _resize(io,ofs,resize)
            return ofs
        end
    end
    # nothing but realloc helps ...
    realloc(io,ofs,resize)
end


function ensure_writeable(io::IOShared, count::Int)
    modify!(io,io.writeofs,count)
    nothing
end


"""
    ensure_readable(io::IOShared, count::UInt64)::Bool

ensure that count bytes can be read from io or throw an error.
"""
function ensure_readable(io::IOShared, count::UInt32)
    usize(io) >= count && return
    if !(io.io isa Nothing)
        fillup(io,count-usize(io)%UInt32)
        usize(io) >= count && return
    end
    error("read request beyond end-of-data",io,count)
end



function fill end


"""
    fillup(io::IO,count::UInt32)

if io try to get count bytes try to make available at least count bytes in buffer of iointo io .
n, the number of bytes read, may vary a lot:

n==0 (because nothing is available),
n < count (because only n bytes are available)
n == count (very unlikely)
n > count (we have more bytes available and more space in io.buffer)

NOOP if IO is flushable or io <:IO{Nothing}
"""
fillup(io::IO,count::UInt32) = nothing

function fillup(io::IOShared{T}, count::UInt32) where T <: IO
    io.flushable && return nothing
    avail = min(bytesavailable(io.io), typemax(UInt32)) % UInt32
    if count > avail
        count = avail
    end
    # count is now fill request, reduced it less is available
    if count > io.limit-io.writeofs
        # need more space
        if io.readofs-io.share + count <= io.limit-io.writeofs
            # we can move in buffer!
            resize(io,io.share,io.share-io.readofs)
        else
            # need new buffer :-(
            realloc(io,io.writeofs,count)
        end
    end
    # enough space to fulfil request: can we fill more than needed?
    if io.limit-io.writeofs > count
        count = min(avail, io.limit-io.writeofs)
    end
    # finally, do fill
    s = io.buffer
    GC.@preserve s unsafe_read(io.io,io.ptr+io.writeofs,count%UInt64)
    io.writeofs += count
    nothing
end


"""
locate a single byte in a buffer.

return typemax(UInt32) if not found or 1st offset found.

"""
function locate(pool::IOShared,s::UInt8)
    size = usize(pool)%Int
    p = pool.ptr+pool.readofs
    b = pool.buffer
    GC.@preserve b
        q = ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, s, size)
    q == C_NULL ? typemax(UInt32) : (q-p)%UInt32
end


minihash(v::UInt8) = (1%UInt64)<<(v&63)


"""
    locate(pool::IOShared,ofs::UInt32,size::UInt64,s::String)

Search in pool for a byte sequence of size bytes, beginning at
offset ofs (0-based!) in String s.

Return the offset (0-based) in pool.buffer or typemax(UInt32) (not found).

TODO use boyer-moore search if size and usize(pool) are 'large'.
TODO search for last 32 bytes if size>32 - that maximizes skip distance
"""
function locate(pool::IOShared,ofs::UInt32,size::UInt64,s::String)
    if size == 0
        return pool.readofs
    end
    if size == 1
        return locate(pool, codeunit(s,ofs+1))
    end
    s_ptr = pointer(s)
    s_end = s_ptr +size
    p_ptr = pool.ptr+pool.readofs
    p_size = usize(pool)
    buffer = pool.buffer
    GC.@preserve buffer s begin
        cs_last = unsafe_load(s_ptr+size-1)
        if size == 1
            # single byte search
            return locate(io, cs_last)
        end
        # TODO: classical brute force search for very small size eg <=3 or 7
        # TODO: search with boyer-moore for larger sizes eg>15
        # search adopted from in _searchindex(s::ByteArray, t::ByteArray, i::Integer)
        # build minihash union
        bloom = 0%UInt64 # ORed minihashes s[1..size-1]
        skip = size # skip if last byte matches, some other byte not
        j = size-1
        # build bloom mask and maximal skip
        while (j-=1) >= 0
            cs = unsafe_load(s_ptr+j)
            bloom |= minihash(cs)
            if cs==cs_last
                skip = min(skip,size-j-1)
            end
        end
        # now bloom is ORed hash of all bytes in s except the last one
        # minihash(byte) & bloom == 0 implies: byte does not occur in s,
        # except last byte.
        # loop for all possible matches of last byte: i=size-1..p_size-1
        i = size-1
        while i < p_size
            cp = unsafe_load(p_ptr+i) # last byte in current match candidate
            #skip = 0 # we have not determined skip distance
            if cp==cs_last
                ## last byte matches. test successive backwards up to i-size+1
                j = size-1
                p0 = p_ptr+i-j # pointer to offset 0 in current match
                while (j-=1) >= 0
                    if unsafe_load(p0+j) != unsafe_load(s_ptr+j)
                        break
                    end
                end
                if j<0
                    #match
                    return i-(size-1) # offset of 1st byte
                end
                # no match.
                i += skip
            elseif bloom & minihash(cp) == 0
                # cp does not occur in s
                i += size
            else
                # default skip: 1 byte
                i += 1
            end
        end
    end
end




## Base API



"""
if io.flushable, write all current contents to data sink.

content is removed from this.
no buffer reorganization, but flushing can increase
re-use of an unshared buffer
"""
function Base.flush(io::IOShared{T}) where T <: IO
    if io.flushable && usize(io) > 0
        twrite(io.io,io.readofs,usize(io), io.buffer)
        io.readofs = io.writeofs
    end
    nothing
end




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
        print(io,SubString(buffer,readofs+1,readofs+5),
        '~',SubString(buffer,size-4,size))
    end
    print(io,"')")
end


function Base.skip(io::IOShared, delta::Integer) 
    pos = io.readofs%Int64 + delta
    if pos>io.writeofs
        fill(io,UInt32(pos-io.writeofs))
    end
    pos < 0 && error("IOShared skip results in illegal position: $pos")
        
    else
    end
    if io.writeofs-io.readofs < d
        fill(io,d)
        if io.writeofs-io.readofs < d
            d = io.writeofs-io.readofs
        end
    end
    io.readofs += d
    return io # same return convention as skip(IOBuffer,..)
end


function Base.skip(io::IOShared, delta::Integer)
    d = UInt32(delta) #  checks negative
    if io.writeofs-io.readofs < d
        fill(io,d)
        if io.writeofs-io.readofs < d
            d = io.writeofs-io.readofs
        end
    end
    io.readofs += d
    return io # same return convention as skip(IOBuffer,..)
end


"""
    Base.read(io::IOShared, ::Type{BToken})

Read a Token, sharing the buffer (no content copying).
This is a main benefit of IOShared over IOBuffer: 
IOShared is still mutable, it keeps track of shared portions 
and reallocates if shared portions have to be changed.

"""
function Base.read(io::IOShared, ::Type{BToken})
    tt = read(io,BufferFly)
    size = usize(t)
    ensure_readable(io,size)
    tt |= io.readofs
    io.readofs += size
    @inbounds BToken(tt,share(io,io.readofs))
end



"read token shared (no string allocation)"
function Base.read(io::IOShared, ::Type{Token})
    p =  read(io,Packed31)
    size = bits4_30(p)
    if (size<=MAX_DIRECT_SIZE)
        return Token(read(io,p,DirectFly))
    end
    ensure_readable(io,size)
    tt = BufferFly(p)
    # reference and skip string data in IOShared instance
    tt |= io.readofs # add offset
    io.readofs += size
    @inbounds Token(hf(tt),share(io,io.readofs))
end


"""
    put(pool::IOShared, t::ParameterizedToken, locate::Bool = false)


put contents of a token into a pool and return a token of the 
same content and type. Noop if content is already in pool or
a direct coded in flytoken. 

If locate is true, a search for the contents in pool is performed,
found contents is referenced. This can reduce memory consumption 
but increases CPU use.

"""
function put(pool::IOShared, t::ParameterizedToken{FLY}, locate::Bool = false) where FLY <:FlyToken
    isdirect(t) && return t # nothing to do on DirectToken
    t.buffer === pool.buffer && return t # nothing to do on already pooled content
    size = usize(t)
    ofs = typemax(UInt32)
    if locate
        ofs = locate(pool,offset(t),size,t.buffer)
    end
    if ofs ==typemax(UInt32)
        # not found: append data
        ensure_writeable(pool,size)
        ofs = pool.writeofs
        write(pool,offset(t),size,t.buffer)
    end
    @inbounds ParameterizedToken{FLY}((t.fly & !OFFSET_BITS) | ofs,share(pool,ofs+size%UInt32))
end


function Base.unsafe_read(from::IOShared, p::Ptr{UInt8}, nb::UInt)
    #from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    ensure_readable(from,nb)
    b = from.buffer
    GC.@preserve b unsafe_copyto!(p, pointer(b)+from.readofs, nb)
    from.readofs += nb
    nothing
end

function Base.unsafe_write(to::IOShared, p::Ptr{UInt8}, nb::UInt)
    ensure_writeable(to,nb)
    b = to.buffer
    GC.@preserve b unsafe_copyto!(to.ptr+to.writeofs, p, nb)
    to.writeofs += nb
    nothing
end


function base.read(from::IOShared, ::Type{UInt8})
    ensure_readable(from,1)
    b = from.buffer
    GC.@preserve b ret = unsafe_load(from.ptr+from.readofs)
    from.readofs += 1
    ret
end


function Base.read(from::IOShared, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64},Type{Int128},Type{UInt128},Type{Float16},Type{Float32},Type{Float64}})
    nb = sizeof(T)
    ensure_readable(from,nb)
    b = from.buffer
    GC.@preserve b begin
        ptr::Ptr{T} = from.ptr+from.readofs
        x = unsafe_load(ptr)
    end
    from.readofs += nb
    return x
end


function Base.eof(io::IOShared)
    if io.readofs >= io.writeofs
        fill(io,1%Uint32)
    end
    return io.readofs >= io.writeofs
end

"no of bytes immediately available for reading "
Base.bytesavailable(io::IOShared) = usize(io)%Int


Base.bytesavailable(io::IOShared{T}) where T <: IO = usize(io)%Int + bytesavailable(io.io)



Base.position(io::IOShared) = io.readofs %Int


# seek makes no sense for queue-like buffers
function Base.seek(io::IOShared{Nothing}, n::Integer)
    # different from IOBuffer which projects on valid offsets, due to a REPL requirement
    (n < 0 || n > io.writeofs) && throw(ArgumentError("Attempted to seek outside IOShared boundaries."))
    io.readofs = n%UInt32
    return io
end


Base.seekstart(io::IOShared{Nothing}) = seek(io,0)


function Base.seekend(io::IOShared)
    io.readofs = io.writeofs
    return io
end




"return up to requested count bytes as Vector"
function Base.read(src::IOshared, count::Integer = typemax(Int))
    size = min (count, bytesavailable(src))
    b = Vector{UInt8}(undef, size)
    readbytes!(src, b, size)
    return b
end


function Base.readbytes!(src::IOShared, b::Vector{UInt8}, count=length(b))
    # resize if necessary
    size = min (count, bytesavailable(src))
    if (size > length(b))
        resize!(b, size)
    end
    unsafe_read(src,pointer(b),size)
    return size
end


function Base.peek(from::IOShared)
    ensure_readable(from,1)
    b = from.buffer
    x = 0%UInt8
    GC.@preserve b begin
        ptr::Ptr{T} = from.ptr+from.readofs
        x = unsafe_load(ptr)
    end
    return x
end


Base.read(from::IOShared, ::Type{Ptr{T}}) where {T} = convert(Ptr{T}, read(from, UInt))


Base.isreadable(io::IOShared) = true
Base.iswritable(io::IOShared) = true




"similar to take!(IOBuffer) but returns a substring instead of a mutable byte array"
function Base.take!(io::IOShared{Nothing})
    ss = SubString(io.readofs,usize(io), shared(io,io.writeofs))
    io.readofs = io.writeofs = 0
    return ss
end


function truncate(io::IOShared{Nothing}, n::Integer)
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


@inline ensureroom(io::IOBuffer, nshort::Int) = ensureroom(io, UInt32(nshort))
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



@inline function write(to::IOShared, a::UInt8)
    ensure_writeable(1)

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
