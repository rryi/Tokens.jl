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
function _resize(io::IOShared, ofs:UInt32, resize:Int)
    @boundscheck begin
        io.shared <= ofs # never chance shared contents
        && ofs <= io.writeofs # not beyond end of content
        && ofs <= io.writeofs + resize  # no delete beyond content (resize<0)
        && io.writeofs+resize <= io.limit # enough space (relevant if resize>0)
        || boundserror(io,ofs, resize)
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
        io.readofs <= ofs && ofs <= io.writeofs
        && size >= 0 && size <= typemax(UInt32)
        && (ofs-resize <= io.writeofs)
        || boundserror(io,ofs,resize)
    end
    # new limit: several considerations.
    # (1) at least, it must be >= size
    # (2) should not be smaller then preferredlimit
    # (3) ensure that write(UInt8) is O(1) ==> an O(1) realloc seldom a constantly growing buffer (by small appends)
    limit = io.preferredlimit
    while size > limit
        # enlarge
        limit <<= 1 # ensures consideration (3): double buffer sizes
        limit == 0 && limit = typemax(UInt32) # handles overflow
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
        fill(io,count-usize(io)%UInt32)
        usize(io) >= count && return
    end
    error("read request beyond end-of-data",io,count)
end



function fill end


"""
    fill(io::IO,count::UInt32)

try to read at least count bytes into io.
n, the number of bytes read, may vary a lot:

n==0 (because nothing is available),
n < count (because only n bytes are available)
n == count (very unlikely)
n > count (we have more bytes available and more space in io.buffer)

NOOP if IO is flushable or io <:IO{Nothing}
"""
fill(io::IO,count::UInt32) = nothing

function fill(io::IO{T}, count::UInt32) where T <: IO
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


minihash(v:UInt8) = (1%UInt64)<<(v&63)


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
        while j-=1 >= 0
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
                while j-=1 >= 0
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
            elseif
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
function Base.flush(io::IOShared{T<:IO})
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
        print(io,SubString(buffer,readofs+1,readofs+5)),
        '~',SubString(buffer,size-4,size))
    end
    print(io,"')")
end


function Base.skip(io:IOShared, delta::Integer)
    d = UInt32(delta) #  checks negative
    if io.writeofs-io.readofs < d
        fill(d)
        if io.writeofs-io.readofs < d
            d = io.writeofs-io.readofs
        end
    end
    io.readofs += d
    return io # same return convention as skip(IOBuffer,..)
end


"read token shared (no string allocation)"
function Base.read(io::IOShared, ::Type{Token})
    t = read(io,::HybridToken)
    size = usize(t)
    if size <= MAX_DIRECT_SIZE
        t = hf(read(io,df(t))
        @inbounds bt = Token(t,EMPTYSTRING)
    else
        @inbounds bt = Token(hf(uint(t)|io.readofs),io.buffer)
        io.readofs += size
        share(io,io.readofs)
    end
    bt
end


"read token shared (no string allocation)"
function Base.read(io::IOShared, ::Type{BToken})
    t = read(io,::HybridToken)
    size = usize(t)
    tt = bf(uint(t)|io.readofs)
    io.readofs += size
    @inbounds  BToken(tt,share(io,io.readofs))
end



"optimized: no string allocation"
function Base.read(io::IOShared, ::Type{Token})
    tt = read(io,HybridFly)
    ss = EMPTYSTRING
    size = usize(tt)
    if size <= MAX_DIRECT_SIZE
        tt = hf(read(io,df(tt))
    else
        # reference and skip string data in IOShared instance
        tt = hf((u64(tt)) | io.readofs) # add offset
        io.readofs += size
        ss = share(io,io.readofs)
    end
    @inbounds Token(tt,ss)
end

"""
    unsafe_tread(from::IOShared, fly::HybridFly) ::Token

read contents of an incomplete HybridFly (only category, size is defined,
other bytes are 0) and returns it as Token.

Method is named unsafe because returned Token references the buffer of from
without explicit sharing. Returned Token might become invalid with the next
operation on from.

This is an internal helper method. Use tread(from,fly) in application code,
which implements proper sharing.
"""
function unsafe_tread(from::IOShared, fly::HybridFly) ::Token
    if isdirect(fly)
        return hf(read(from,fly))
    end
    size = usize(fly)
    ensure_readable(from, size%UInt32)
    fly = fly | from.readofs
    from.readofs += size
    Token(fly,from.buffer)
end




"""
read a Token using size and category from an incomplete HybridFly
"""
function tread(from::IOShared, t::HybridFly) ::Token
    ret = unsafe_tread(from,t)
    share(from,from.readofs)
    ret
end



"""
    Base.read(io::IOShared, fly::HybridFly, pool::IOShared, intern::Bool=false)

read contents for an incomplete  HybridFly (category and size is defined,
all other bytes are 0). Such a HybridFly is constructed by HybridFly(category,size)
or by reading a hybridFly from some IO.

If fly is a DirectFly, return it with contents read from io.
Otherwise, read contents and store it in pool, without sharing it with io.
If intern is true, pool is searched if it already contains contents.
If not found, contents is appended to pool.
The offset in pool is returned within the HybridFly.

This method is intended for a scenario where sharing of io is ineffective and
should be avoided.
It avouds copying the bufer twiceto be avoided
found, contents is appended to pool
"""
function put(pool::IOShared, t::GenericToken, intern::Bool = false)
    isdirect(t) && return t # nothing to do on DirectToken
    size = usize(t)
    ofs = typemax(UInt32)
    if intern
        ofs = locate(pool,offset(t),size,t.buffer)
    end
    if ofs ==typemax(UInt32)
        unsafe_write
        ensure_writeable(pool,size)
        ofs = pool.writeofs

        pool.share(ofs+size%UInt32)
        return Token((t.fly & !OFFSET_BITS) | ofs),pool.share(ofs+size%UInt32))
    end
    twrite(pool,from.readofs,size,from.buffer)


    tt = read(io,HybridFly)

    ss = EMPTYSTRING
    size = usize(tt)
    if size <= MAX_DIRECT_SIZE
        return hf(read(io,df(tt))
    else
        if intern
            ofs = locate(buffer,io.readofs,usize(tt),io.buffer)
        # reference and skip string data in IOShared instance
        tt = hf((u64(tt)) | io.readofs) # add offset
        io.readofs += size
        ss = share(io,io.readofs)
    end
    @inbounds Token(tt,ss)
end


function tread(io::IOShared, t::DirectFly) ::DirectFly
    size = usize(t)
    ensure_readable(size)
    b = io.buffer
    GC.preserve b begin
        for i in 1:size
            t = unsafe_setcodeunit(t,i,unsafe_load(io.ptr+io.readofs)
            io.readofs += 1
        end
    end
    t
end


# serializing of DirectFly
function Base.write(io::IOShared, t::DirectFly)
    write(io,hf(t))
    size = usize(t)
    ensure_writeable(size)
    b = io.buffer
    GC.preserve b begin
        for i in 1:size
            unsafe_store!(io.ptr+io.writeofs,@inbounds codeunit(t,i))
            io.writeofs += 1
        end
    end
end


function Base.read(io::IOShared, ::Type{Token})
    t = read(io,HybridFly)
    size = usize(t)
    if size <= MAX_DIRECT_SIZE
        t = hf(read(io,df(t)))
        return @inbounds Token(t,EMPTYSTRING)
    end

        @inbounds Token(t, read(io,size,String))
    end

    tt = read(io,HybridToken)
    if isdirect(tt)
        # TODO can we reference bytes in io instance in this case??
        # possibly dependent on endianness...
        BToken(df(tt))
    else
        # reference buffer
        tt = bf(u64(tt)& ~OFFSET_BITS)
        size = usize(tt)
        tt = hf((u64(tt)& ~OFFSET_BITS) | ofs)
        ss = io.buffer
        io.readofs += size
        @inbounds BToken(bf(tt), t.ss)
    end
end

function Base.read(io::IOShared, ::Type{BToken})
    t = read(io,Token)
    tt = t.tiny
    if isdirect(tt)
        # TODO can we reference bytes in io instance in this case??
        # possibly dependent on endianness...
        BToken(df(tt))
    else
        # reference buffer
        tt = bf(u64(tt)& ~OFFSET_BITS)
        size = usize(tt)
        tt = hf((u64(tt)& ~OFFSET_BITS) | ofs)
        ss = io.buffer
        io.readofs += size
        @inbounds BToken(bf(tt), t.ss)
    end
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
    GC.@preserve b unsafe_copyto!(to.ptr+to.writeofs), p, nb)
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

function read(from::IOShared, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64},Type{Int128},Type{UInt128},Type{Float16},Type{Float32},Type{Float64}})
    nb = sizeof(T)
    ensure_readable(nb)
    b = from.buffer
    GC.@preserve b begin
        ptr::Ptr{T} = from.ptr+from.readofs
        x = unsafe_load(ptr)
    end
    from.readofs += nb
    return x
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
