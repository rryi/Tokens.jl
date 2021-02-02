# This file is derived from julia iobuffer.jl. License is MIT: https://julialang.org/license

## work with String as UInt8 buffer via I/O primitives and share with tokens/substrings ##

# Stateful string

const MaybeIO = Union{Nothing, IO}

"a constant for a default allocation limit in IOShared and other resizable array structures"
const DEFAULTLIMIT = 1024%UInt32

"value indicating no mark ist set"
const NOMARK =typemax(UInt32)


"""
IO buffer which can share content with SubString and AbstractToken instances.

Reading of tokens or substrings can be done without heap allocations - 
such returned items simply share the internal buffer, IOShare instance 
will "copy on write" to protect shared data.

IOShared comes in three flavours:

 * IOShared{Nothing}: closely resembles IOBuffer with some additional 
 text processing methods giving a "mutable string" behavior, readableand writable

 * IOShared{T<:IO}, ioread: buffered input stream.
 readable but not writeable, internal buffer is filled from a data source
 attached by constructor.
 
 * IOShared{T<:IO}, !ioread: buffered output stream.
 All data written go to a data sink attached by constructor when flush is called or
 if internal buffer space is exhausted.

All flavours maintain two internal offsets, marking the currently valid part of the internal buffer.
One is the current read position, the other the current write position. 

IOShared{Nothing} can be used as byte queue (intermittent reads and writes). 
Current content is only changed by explicit content operations (read,
write, resize and some more). Offsets, however, are also implicitly changed, 
so do not rely on offset stability after operations.

IOshared{IO} will chance buffer content implicitly: with attached data sink,
operations can cause flushing of current content to data sink. With attached 
data source, read operations may trigger appending data from source to 
internal buffer. Time and size of those implicit content changes are under
control of the IOBuffer implementation.

For IOshared{IO} With attached data source, writing is technically allowed, but unusual:
writing to a buffered input stream will effectively insert data at the current 
write position, not at the end of input data. 

Similar for IOshared{IO} with an attached data sink, reading from it is allowed 
but will delete content before it is written to sink.

You can covern implicit content changes by use of the mark mechanism: setting the
mark to a content position, effectively prevents implicit content changes
between mark position and write position.  However content before the mark
position may get removed by implicit changes, moving all offsets.
To also prevent this, set mark position to 0. This and only this guarantees that
content offsets will be changed by explicit operations, only.
"""
mutable struct IOShared{T <: MaybeIO} <: IO
    buffer :: String # PRIVATE!! memory with byte data.
    ptr :: Ptr{UInt8} # PRIVATE!! pointer to buffer.
    readofs :: UInt32 # read position offset / number of consumed bytes
    writeofs :: UInt32 # write (append) position offset / end of defined data
    shared :: UInt32 # offset in buffer behind last shared byte (0: nothing shared)
    mark :: UInt32 # offset of data not to flush/forget. typemax(UInt32): no active mark
    limit ::  UInt32 # total number of bytes in buffer
    preferredlimit ::  UInt32 # limit in case of reallocation due to sharing
    io::T # nothing or if ioread: source for fillup, else target for flush
    ioread :: Bool # true: io is source for fillup. false: io is target for flush or nothing
    locateOnPut :: Bool # true: put does lookup content, share instead of append if found
    function IOShared{T}(io::T,ioread::Bool,limit::UInt32) where T <: MaybeIO
        z = zero(UInt32)
        s = _string_n(limit)
        new(s,pointer(s),z,z,z,NOMARK,limit,limit,io,ioread,false)
    end
    function IOShared{Nothing}(offset::UInt32, size::UInt64, s::String)
        @boundscheck checkrange(offset,size,s)
        size32= UInt32(size) # check on overflow necessary: cannot handle too long strings
        preferredlimit = max(size32,DEFAULTLIMIT) # well ... just a very simple guess
        limit = UInt32(ncodeunits(s)) # check on overflow necessary: cannot handle too long strings
        new(s,pointer(s),offset,offset+size32,limit,NOMARK,limit,preferredlimit,nothing,false,false)
    end
end

IOShared(io::T,ioread:Bool, limit::UInt32=DEFAULTLIMIT) where T <: MaybeIO = IOShared{T}(io,ioread,limit)

IOShared(limit::UInt32=DEFAULTLIMIT) = IOShared(nothing,false,limit)

IOShared(s::SubString{String}) = IOShared{Nothing}(UInt32(s.offset),s.ncodeunits%UInt64,s.string)

IOShared(s::String) = IOShared{Nothing}(zero(UInt32),(ncodeunits(s)%UInt64),s)

IOShared(t::BToken) = IOShared{Nothing}(offset(t.tiny),usize(t.tiny),t.buffer)

IOShared(t::Token) = IOShared(BToken(t))



## internal API

#"offset boundscheck, including "
#Base.checkbounds(::Type{Bool}, io::IOShared, ofs::UInt32) = io.readofs <= ofs <= io.writeofs

"""
    _share(io::IOShared,share::UInt32) :: String

Return a buffer reference. Like take!, this function gives you efficient access to
the whole content of io, but unlike take!, io continues using buffer also for write
operations. 

That buffer is typed as String, but somehow violates String immutability:
only the first share bytes in the returned buffer are guaranteed not to change.
If io needs to change its buffer in the first share bytes, it allocates a
new buffer and copies needed content to it.

Of course, the returned String must  be treated immutable - writing to it 
via pointer operations may result in system crashes.

_share is regarded a private function. To obtain the text content of an IOShared,
use read operations.
"""
function _share(io::IOShared,share::UInt32)
    if (io.shared<share)
        io.shared = share
    end
    return io.buffer
end


"share the whole internal buffer. ATTENTION: may contain uninitialized data"
_share(io::IOShared) = _share(io,io.limit)


"currently loaded content size in bytes. Changes e.g. on fill, flush, read, write"
usize(io::IOShared) = (io.writeofs-io.readofs) % UInt64


"""
move memory within a IOShared buffer: insert/delete at ofs

Method is regarded private and intended to be used within IOShared, only.
External API is function resize.

ofs is internal offset into io-s buffer.
PRECOND: limit large enough, no share at ofs, no delete beyond io.writeofs
ofs <= io.writeofs, if resize<0: ofs-resize <= io.writeofs

readofs, writeofs and mark are adjusted in io. 
No reallocation - if resize would violate sharing, an error is thrown.
"""
function _resize(io::IOShared, ofs::UInt32, resize::Int)
    @boundscheck begin
        (io.shared <= ofs # never chance shared contents
        && ofs <= io.writeofs # not beyond end of content
        && ofs <= io.writeofs + resize  # no delete beyond content (resize<0)
        && io.writeofs+resize <= io.limit # enough space (relevant if resize>0)
        ) || error("preconditions violated: resize by $resize at $ofs in $io")
    end
    s = io.buffer
    GC.@preserve s begin
        if resize>0 #insert is the most frequently used case
            ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                  io.ptr+ofs+resize, io.ptr+ofs, io.writeofs-ofs)
            
        else #delete/noop
            resize == 0 && return nothing
            ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                  io.ptr+ofs, io.ptr+ofs-resize, io.writeofs-ofs+resize)
        end
    end
    io.writeofs += resize
    ofs<io.readofs && (io.readofs += resize)
    io.mark != NOMARK && ofs<io.mark && (io.mark += resize)
    nothing
end


"""
allocate a new buffer and preserve content, resizing at offset ofs.

Method tries to flush first, to reduce allocation as well as
the amount of bytes to copy.

io is changed, adjusted ofs is returned.

Inserted bytes (resize>0) are uninitialized.

ofs precondition: io.readOfs <= ofs <= io.writeOfs

realloc is used internally, external use is allowed but
not recommended.

"""
function realloc!(io::IOShared, ofs::UInt32, resize::Int) ::UInt32
    flush(io)
    preservedofs = min(io.readofs, io.mark) # begin of protected content which must be copied
    size = io.writeofs - preservedOfs + resize
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
    # (3) ensure that write(UInt8) is O(1) ==> enlarge size such that free space is O(size)
    limit = io.preferredlimit
    while size > limit
        # enlarge
        limit <<= 1 # ensures consideration (3): double buffer sizes
        limit == 0 && (limit = typemax(UInt32)) # handles overflow
    end
    buf = _string_n(limit)
    ptr = pointer(buf)
    count = (ofs - preservedofs) %UInt # size of reserved content before ofs
    s = io.buffer # only for @preserve
    GC.@preserve s buf # for safety. might be unnecessary
    begin
        ## copy first part
        if count>0
            ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                  ptr, io.ptr+preservedofs, count)
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
    ofs -= preservedofs
    io.readofs -= preservedofs
    io.mark != NOMARK && (io.mark -= preservedofs)
    io.shared = 0
    io.writeofs = size %UInt32
    io.limit = limit
    io.buffer = buf
    io.ptr = ptr
    return ofs
end


"""

    modify!(io::IOShared, ofs::UInt32, resize::Int)

Ensure that io.buffer is writeable starting at ofs, and resize current content at ofs.

If resize is negative, deletion is restricted to the current content.
If io.shared>ofs, or if io.limit < ofs+resize, a new buffer is allocated.

Buffer allocation may change offsets, i.e. io.readofs, io.writeofs and ofs.
Instance variables are adjusted, the adjusted value of ofs is returned by modify.

Return adjusted ofs

Method is used package-internally, but also exported as central low level
resize method.
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
        preservepos = min(io.readofs,io.mark)
        delta = io.shared%Int - preservepos # bytes deleteable at shared if <0
        if io.writeofs+resize+delta <= io.limit
            # removing obsolete content after shared is enough
            _resize(io,io.shared,delta) # remove free space at beginning
            ofs += delta
            _resize(io,ofs,resize)
            return ofs
        end
    end
    # nothing but realloc helps ...
    return realloc(io,ofs,resize)
end


function ensure_writeable(io::IOShared, count::UInt32)
    modify!(io,io.writeofs,count%Int)
    return nothing
end

function Base.ensureroom(io::IOShared, nshort::Int) 
    ensure_writable(io, UInt32(nshort))
    return io
end


"""
    ensure_readable(io::IOShared, count::UInt64)::Bool

ensure that count bytes can be read from io or throw an error.
"""
function ensure_readable(io::IOShared{T}, count::UInt32) where T <: MaybeIO
    fillup(count)
    io.writeofs-io.readofs >= count && return nothing
    if !(T===Nothing)
        fillup(io,count-(io.writeofs-io.readofs))
        usize(io) >= count && return nothing
    end
    error("read request for $count bytes fails - end-of-data")
end


"""
    fillup(io::IO,count::UInt32)

try to ensure that count bytes are readable from io.
In contrast to  [`ensure_readable`](@ref), no error
is thrown if not successful.

if io has an attached data source, and usize(io)<count,
fillup will try to put additional count-usize(io) bytes 
from data source into buffer of io.

n, the effectively number of bytes which are transferred from data source, 
may vary a lot:

 * n==0 (because nothing is available),
 
 * 0 < n < count-usize(io) (because only n bytes are available)
 
 * n == count-usize(io) (very unlikely)

 * n > count-usize(io) (data sink has more than n bytes,
   and we can put more than count bytes into io without reallocation)

fillup is called internally on read requests to ensure enough
bytes are available in io-s buffer.

Search operations on IOBuffer never do fillup internally. To get
valid search results, you must call fillup before searching with
a count so large that match results beyond count bytes are
irrelevant.

NOOP if io <:IO{Nothing} or io.ioread==false
"""
fillup(io::IOShared{Nothing},count::UInt32) = nothing

function fillup(io::IOShared{T}, count::UInt32) where T <: IO
    if io.ioread && io.writeofs-io.readofs < count
        count -=  io.writeofs-io.readofs # what is additionally requested 
        avail = min(bytesavailable(io.io), typemax(UInt32)) % UInt32
        count > avail && (count = avail)
        # count is now fill request, reduced if less is available
        ensure_writable(io,count)
        # can we fill more than requested?
        if io.limit-io.writeofs > count
            count = min(avail, io.limit-io.writeofs)
        end
        # finally, do fill
        s = io.buffer
        GC.@preserve s unsafe_read(io.io,io.ptr+io.writeofs,count%UInt)
        io.writeofs += count
    end
    nothing
end



## searching (w/o Matcher)
## TODO locate umbenennen in findfirst?
## TODO wir brauchen Suche nach byte, Token, Substring. 
## TODO wir sollten das auf SubString implementieren und IOShared auf SubString 'casten'


"""
locate a single byte in a buffer.

return typemax(UInt32) if not found or 1st offset found.

"""
function locate(pool::IOShared,pattern::UInt8)
    size = usize(pool)%Int
    p = pool.ptr+pool.readofs
    b = pool.buffer
    GC.@preserve b
        q = ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, pattern, size)
    q == C_NULL ? typemax(UInt32) : (q-p)%UInt32
end


minihash(v::UInt8) = (1%UInt64)<<(v&63)
#TODO an best alg anpassen.
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



"fast search in buffer (no fillup!)"
function Base.occursin(delim::UInt8, io::IOShared)
    buf = io.buffer
    q = GC.@preserve buf ccall(:memchr,Ptr{UInt8},(Ptr{UInt8},Int32,Csize_t),io.ptr+io.readofs,delim,usize(io))
    return q != C_NULL
end


Base.match(r::Regex, io::IOShared) = match(r,substr(io))

 

# unfertig!!! 
function Base.readuntil(io::IOShared, delim::UInt8; keep::Bool=false)
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



## Base API



"""
if not io.ioread, write all unmarked current contents to data sink.

written content is removed from this.
no buffer reorganization, but flushing can increase
re-use of an unshared buffer
"""
function Base.flush(io::IOShared{T}) where T <: MaybeIO
    if ! (T===Nothing) && !io.ioread
        endofs = min(io.mark,io.writeofs)
        if  endofs > io.readofs
            sswrite(io.io,io.readofs,endofs-io.readofs, io.buffer)
            io.readofs = endofs
        end
    end
    nothing
end


"return a new IOBuffer with same (shared) contents, but no mark, default preferredlimit"
Base.copy(b::IOShared{Nothing}) = IOShared(io.readofs,usize(io),_share(io,io.limit))



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


Base.skip(io::IOShared, delta::Integer) = seek(io,io.readofs+delta)



"""
    put(pool::IOShared, t::ParameterizedToken, locate::Bool = false)


put contents of a token into a pool and return a token of the 
same content and type. Noop if content is already in pool or
a direct coded in flytoken. 

If pool.locateOnPut is true, a search for the contents in pool is performed,
found contents is referenced. This can reduce memory consumption 
but increases CPU use. Default is false, change it with put(pool,true)

"""
function put(pool::IOShared{Nothing}, t::ParameterizedToken{FLY}) where FLY <:FlyToken
    isdirect(t) && return t # nothing to do on DirectToken
    t.buffer === pool.buffer && return t # nothing to do on already pooled content
    size = usize(t)%UInt32
    ofs = typemax(UInt32)
    if pool.locateOnPut
        ofs = locate(pool,offset(t),size,t.buffer)
    end
    if ofs ==typemax(UInt32)
        # not found: append data
        ensure_writeable(pool,size)
        ofs = pool.writeofs
        write(pool,offset(t),size,t.buffer)
    end
    @inbounds ParameterizedToken{FLY}((t.fly & !OFFSET_BITS) + ofs,_share(pool,ofs+size))
end

"set pool.locateOnPut for subsequent put calls"
function put(pool::IOShared{Nothing}, locateOnPut::Bool) 
    pool.locateOnPut = locateOnPut
end


## read functions


function Base.unsafe_read(from::IOShared, p::Ptr{UInt8}, nb::UInt)
    #from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    ensure_readable(from,nb)
    b = from.buffer
    GC.@preserve b unsafe_copyto!(p, pointer(b)+from.readofs, nb)
    from.readofs += nb
    nothing
end

"""
    Base.read(io::IOShared, ::Type{BToken})

Read a Token, sharing the buffer (no content copying).
This is a main benefit of IOShared over IOBuffer: 
IOShared is still mutable, it keeps track of shared portions 
and reallocates if shared portions have to be changed.

"""
function Base.read(io::IOShared, ::Type{BToken})
    p = read(io,Packed31)
    size = usize(t)
    ensure_readable(io,size)
    t = BufferFly(p) + io.readofs
    io.readofs += size
    @inbounds return BToken(t,_share(io,io.readofs))
end



"read token shared/direct (no string allocation)"
function Base.read(io::IOShared, ::Type{Token})
    p =  read(io,Packed31)
    size = bits4_30(p)
    if (size<=MAX_DIRECT_SIZE)
        return Token(read(io,p,DirectFly))
    end
    ensure_readable(io,size)
    t = BufferFly(p) + io.readofs
    # reference and skip string data in IOShared instance
    io.readofs += size
    @inbounds return Token(t,_share(io,io.readofs))
end


"read bytes as a shared substring. Similar available as a peek method"
function Base.read(io::IOShared, ::Type{SubString}, size::UInt64)
    ensure_readable(io,size)

    t = BufferFly(p) + io.readofs
    # reference and skip string data in IOShared instance
    ofs = io.readofs
    io.readofs += size
    @inbounds return substring(ofs,size,_share(io,io.readofs))
end

"read all content in buffer as a SubString (no fillUp!)"
Base.read(io::IOShared, ::Type{SubString}) = read(io,usize(io))


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


# not efficient for IOShared - consider read(SubString)
"return up to requested count bytes as Vector"
function Base.read(src::IOShared, count::Integer = typemax(Int))
    size = min(count, bytesavailable(src))
    b = Vector{UInt8}(undef, size)
    readbytes!(src, b, size)
    return b
end


Base.read(from::IOShared, ::Type{Ptr{T}}) where {T} = convert(Ptr{T}, read(from, UInt))


function Base.read(from::IOShared, ::Type{UInt8})
    ensure_readable(from,1)
    b = from.buffer
    GC.@preserve b ret = unsafe_load(from.ptr+from.readofs)
    from.readofs += 1
    return ret
end


#= inherit it from IO!
function Base.readbytes!(src::IOShared, b::Vector{UInt8}, count=length(b))
    # resize if necessary
    size = min(count, bytesavailable(src))
    if (size > length(b))
        resize!(b, size)
    end
    unsafe_read(src,pointer(b),size)
    return size
end
=#

## write functions


function Base.unsafe_write(to::IOShared, p::Ptr{UInt8}, nb::UInt)
    ensure_writeable(to,nb)
    b = to.buffer
    GC.@preserve b unsafe_copyto!(to.ptr+to.writeofs, p, nb)
    to.writeofs += nb
    return nothing
end


function Base.write(to::IOShared, byte::UInt8)
    ensure_writeable(to,1)
    b = to.buffer
    GC.@preserve b unsafe_store!(to.ptr+to.writeofs, byte)
    to.writeofs += 1
    return nothing
end



"write all available bytes (no waiting for async streams)"
function Base.write(to::IOShared, from::IO)
    # would work, but allocates intermediate Vecior #  = write(to,read(from))
    b = to.buffer
    size = UInt32(bytesavailable(from))
    ensure_writeable(to,size)
    GC.@preserve b unsafe_read(from,to.ptr+to.writeofs,size)
    to.writeofs += size
    return nothing
end


"write all available bytes in from's buffer (no fillup)"
function Base.write(to::IOShared, from::IOShared) 
    to===from && error("write(io,io) not allowed for io::IOShared ")
    sswrite(to,from.readofs,usize(from),from.buffer)
end



# size related operations


function Base.eof(io::IOShared)
    if io.readofs >= io.writeofs
        fillup(io,1%Uint32)
    end
    return io.readofs >= io.writeofs
end

"no of bytes immediately available for reading "
Base.bytesavailable(io::IOShared{Nothing}) = usize(io)%Int


Base.bytesavailable(io::IOShared{T}) where T <: IO = usize(io)%Int + bytesavailable(io.io)



## offset related operations

Base.position(io::IOShared) = io.readofs %Int


"""
    seek(io::IOShared, n::Integer)

Seek moves read position given by position(io) to a new offset.

Please take into account that read and write operations may move
content within the internal buffer, offsets are not stable.

Use seek within currently defined content, given by the offsets 
from position(io) up to position(io)+usize(io) directly before seek operation,
or use mark to "freeze" content above mark position,. and its offsets 
relative to mark position. If mark is 0, all offsets are "frozen" 
except for explicit resize operations.
"""
function Base.seek(io::IOShared, n::Integer)
    # different from IOBuffer which projects on valid offsets, due to a REPL requirement
    (n < 0 || n > io.writeofs) && throw(ArgumentError("Attempt to seek outside IOShared boundaries."))
    io.readofs = n%UInt32
    return io
end


Base.seekstart(io::IOShared{Nothing}) = seek(io,0)


function Base.seekend(io::IOShared)
    io.readofs = io.writeofs
    return io
end


function Base.mark(io::IOShared)
    io.mark = io.readofs
end


Base.ismarked(io::IOShared) = io.mark != NOMARK


function Base.unmark(io::IOShared)
    ret = ismarked(io)
    io.mark = NOMARK
    return ret
end


function Base.reset(io::IOShared)
    io.mark == NOMARK && error("IOShared is not marked")
    io.readofs = io.mark
    io.mark = NOMARK
    return io.readofs
end


"""
    truncate(io::IOShared, n::Integer)

Seek operation for write position. truncate can move write position behind 
current write position, this is equivalent to writing 0x00 bytes such that 
write position becomes n.

If the new write position is smaller than the mark position, 
mark position is removed.

n is a 0-based offset. It is declared Integer to be compatible with 
the method signature in Base, value must be unsigned.
"""
function Base.truncate(io::IOShared{Nothing}, n::Integer)
    n < 0 && throw(ArgumentError("truncate failed, n must be ≥ 0, got $n"))
    pos = UInt32(n) # does check for 32 bit overflow
    if pos >io.writeofs 
        ensure_writeable(io,pos-io.writeofs)
        for i in io.writeofs:pos
            write(io,0x00)
        end
    end
    io.writeofs = pos
    io.readofs>io.writeofs && io.readofs = io.writeofs
    io.writeofs < io.mark && io.mark != NOMARK && unmark(io)
    return io
end


"lookahead 1 byte"
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


"lookahead n bytes, returned as a shared substring"
function Base.peek(io::IOShared, size::UInt64)
    ensure_readable(io,size)
    # reference string data in IOShared instance
    @inbounds return substring(io.readofs,size,_share(io,io.readofs+size))
end

## other IO function overloads for IOShared

Base.isreadable(io::IOShared) = true
Base.iswritable(io::IOShared) = true


"similar to take!(IOBuffer) but returns a substring instead of a mutable byte array"
function Base.take!(io::IOShared{Nothing})
    ss = SubString(io.readofs,usize(io), shared(io,io.writeofs))
    io.readofs = io.writeofs = 0
    return ss
end


@inline Base.ensureroom(io::IOBuffer, nshort::Int) = ensurewritable(io, UInt32(nshort))


@noinline function Base.close(io::IOShared{T}) where T <: IO
    flush(io)
    close(io.io)
    io.writeofs = 0
    io.limit = 0 # enforces reallocation on any attempt to use again
    io.readofs = 0
    io.mark = typemax(UInt32)
    nothing
end

isopen(io::IOShared) = io.limit==0

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


