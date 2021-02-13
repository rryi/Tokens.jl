# This file is derived from julia iobuffer.jl. License is MIT: https://julialang.org/license

## work with String as UInt8 buffer via I/O primitives and share with tokens/substrings ##

# Stateful string

const MaybeIO = Union{Nothing, IO}

"a constant for a default allocation limit in IOShared and other resizable array structures"
const DEFAULTLIMIT = 1024%UInt32

"value indicating no mark ist set"
const NOMARK =typemax(UInt32)

"value indicating no mark ist set"
const NOMARK =typemax(UInt32)


"""
IO buffer which can share content with SubString and Token instances.

Reading of tokens or substrings can be done without heap allocations - 
such returned items simply share the internal buffer, IOShare instance 
will "copy on write" to protect shared data.

An IOShared instance operates in one of the following, mutually exclusive, modes:

1) *IO queue:* like IOBuffer, readable and writable. with some additional 
text processing methods giving a "mutable string" behavior.

2) *buffered input stream:* a data source (another IO instance) is attached.
 When a read operation requests more bytes than available in the internal
 buffer, data from attached source is appended, and already read content
 is removed if not blocked by a mark. IOShared is usually only read.
 Writing is possible, but unusual: if will effectively insert data into the
 input stream at the current write offset position, which is under control 
 of IOShare. More realistic is insertion at the read position, e.g. to 
 replace some include notation by the content to include.

3) *buffered output stream:* a data sink (another IO instance) is attached.
 When a write operation requests more bytes than free space is available 
 in the internal buffer, all content is written to attached data sink and
 then removed if not blocked by a mark. IOShared is usually only written.
 Read access is permitted, but will effectively delete data from the
 output stream. Similar to 2), insert/delete operations are more relevant
 to manipulate the stream befor it gets finally written to the sink. 
 Use mark to ensure that content behind mark is not written to data sink,
 but remove mark as soon as possible.

4) *heap for flyweight tokens:* vectors of flyweight tokens are attached, using
 IOShared for all of their content not directly stored in the flyweight. 
 IOBuffer is neither readable nor writeable in the usual IO sense.
 Token contents get into IOShared with the [`put`](@ref) function, which
 ensures that offsets of all registered flyweight tokens remain valid. 
 Internal reorganization still happens, but will adjust offsets of all 
 registered flyweight tokens.
 
All flavours maintain two internal offsets, marking the currently used part of the internal buffer.
One is the current read position, the other the current write position. 

IOShared queues allow (intermittent reads and writes). 
Current content is only changed by explicit content operations (read,
write, resize and some more). Offsets into content, however, can change, due
to reorganization. Already read content (before readofs) can be removed, reducing
offsets of remaining content.

IOshared buffered streams have only a possibly small window of the whole
data in its internal buffer. Reading from a buffered output stream is
tecnically possible, but will effectively delete data  the stream: read data is no more written
to the data sink. Similarly, writing to a buffered input stream will insert
data into the stream, which is usually not intended. it removes data before  data in data sink,
operations can cause flushing of current content to data sink. With attached 
data source, read operations may trigger appending data from source to 
internal buffer. Time and size of those implicit content changes are under
control of the IOBuffer implementation.

You can govern implicit content changes by use of the mark mechanism: setting the
mark to a content position, effectively prevents implicit content changes
between mark position and write position.  However content before the mark
position may get removed by implicit changes, moving all offsets.
To also prevent this, set mark position to 0. This and only this guarantees that
content offsets will be changed by explicit operations, only.
"""
mutable struct IOShared <: IO
    buffer :: String # PRIVATE! memory with byte data.
    ptr :: Ptr{UInt8} # PRIVATE!!! pointer to buffer. # TODO ersetzen durch pointer call?!!!
    readofs :: UInt32 # read position offset / number of consumed bytes
    writeofs :: UInt32 # write (append) position offset / end of defined data
    shared :: UInt32 # offset in buffer behind last shared byte (0: nothing shared)
    mark :: UInt32 # offset of data not to flush/forget. typemax: no active mark
    limit ::  UInt32 # total number of bytes in buffer
    preferredlimit ::  UInt32 # limit in case of reallocation due to sharing
    io::MaybeIO # nothing or if ioread: source for fillup, else target for flush
    ioread :: Bool # true: io is source for fillup. false: io is target for flush or nothing
    locateOnPut :: Bool # true: put does lookup content, share instead of append if found
    registered :: Vector{HybridFly} # true: mark is always 0 (offsets are guaranteed not to change)
    function IOShared(io::MaybeIO,ioread::Bool,limit::UInt32) 
        ioread && io===nothing && error("ioread must be false if io is nothing")
        z = zero(UInt32)
        s = _string_n(limit)
        new(s,pointer(s),z,z,z,NOMARK,limit,limit,io,ioread,false,Vector{HybridFly}())
    end
    function IOShared(offset::UInt32, size::UInt64, s::String)
        @boundscheck checkrange(offset,size,s)
        size32= UInt32(size) # check on overflow necessary: cannot handle too long strings
        preferredlimit = max(size32,DEFAULTLIMIT) # well ... just a very simple guess
        limit = UInt32(ncodeunits(s)) # check on overflow necessary: cannot handle too long strings
        new(s,pointer(s),offset,offset+size32,limit,NOMARK,limit,preferredlimit,nothing,false,false,Vector{HybridFly}())
    end
end

IOShared(io::MaybeIO,ioread::Bool, limit::UInt32=DEFAULTLIMIT) = IOShared(io,ioread,limit)

IOShared(limit::UInt32=DEFAULTLIMIT) = IOShared(nothing,false,limit)

IOShared(s::SubString{String}) = IOShared(UInt32(s.offset),s.ncodeunits%UInt64,s.string)

IOShared(s::String) = IOShared(zero(UInt32),(ncodeunits(s)%UInt64),s)

IOShared(t::BToken) = IOShared(offset(t.tiny),usize(t.tiny),t.buffer)

IOShared(t::AbstractString) = IOShared(BToken(t))



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
usize(io::IOShared) = usize32(io) % UInt64


usize32(io::IOShared) = io.writeofs-io.readofs

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
    unsafe_put(pool::IOShared, t::Token{F})


put contents of t into pool and return a [`FlyToken`](@ref) referencing
this IOBuffer with the same content.

Is marked unsafe, because there is no share protection for the returned
FlyToken. Method is used internally when storing in a registered [`TokenVector`](@ref),
which does not need share protection.

Use [`put`](@ref) function to get a share-protected Token back.

If pool.locateOnPut is true, a search for the contents in pool is performed,
found contents is referenced. This can reduce memory consumption 
but increases CPU use. Default is false, change it with put(pool,true)
"""
function unsafe_put(pool::IOShared, t::Token{F}) where F
    size = usize(t)%UInt32
    ret = t.fly
    if !isdirect(t) && size>0 && t.buffer !== pool.buffer
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
        ret = (t.fly & !OFFSET_BITS) + ofs
    end
    return ret
end


"""
    put(pool::IOShared, s, ::TOKEN{F})

s can be anything for which a Token{F} constructor exists, 
in particular AbstractString, AbstractToken, Real, Integer, Bool.

put contents of s into pool and return a [`Token`](@ref) with the 
same content. If s isa AbstractToken, its category is preserved, 
for any other AbstractString type, category is set to T_TEXT.
Noop if content is already in pool or direct coded in returned Token. 

If pool.locateOnPut is true, a search for the contents in pool is performed,
found contents is referenced. This can reduce memory consumption 
but increases CPU use. Default is false, change it with put(pool,true)

Using put will switch pool to an offset-preserving mode, which 
disables mark functionality.

put is intended for use cases where  [`FlyToken`](@ref)s are used with known
content pools. Most important case is [`TokenVector`](@ref).
"""
function put(pool::IOShared, s, ::Type{Token{F}}) where F
    f = unsafe_put(pool, Token{F}(s))
    t = Token{F}(f,shared(pool,pool.writeofs))
    return t
end


"set pool.locateOnPut for subsequent put calls"
function put(pool::IOShared, locateOnPut::Bool) 
    pool.locateOnPut = locateOnPut
    return nothing
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
    tokencount = 0 # compute count of registered tokens which occupy heap space
    if length(io.registered)>0
        # FlyToken heap usage: effective size is not given by readofs/writeofs but by FlyToken sizes on heap
        ofs!=io.writeofs || resize<0 && error("only append is allowed for IOShared with registered FlyToken vectors!")
        size = resize # sum up all references into heap: maximum space needed.
        for v in io.registered
            for t in v
                # ignore all direct/empty tokens
                if !isdirect(t) && usize(t)>0
                    size += usize(t)
                    tokencount += 1
                end
            end
        end
    end

    # new limit: several considerations.
    # (1) at least, it must be >= size
    # (2) should not be smaller than preferredlimit
    # (3) ensure that write(UInt8) is O(1) ==> enlarge size such that free space is O(size)
    limit = max(io.preferredlimit,UInt32((size*3)>>1)) # 50% more than necessary size
    buf = _string_n(limit)
    ptr = pointer(buf)
    if tokencount>0
        # FlyToken heap usage: copy all FlyToken content to new buffer
        tmp = IOShared(limit)
        splitsize = ((size-resize)*3 ÷ tokencount) >>2 # 3/4 of average size
        if io.locateOnPut && tokencount>2 && splitsize>1
            runs = [splitsize%UInt64,0%UInt64]
        else
            runs = [0%UInt64]
        end
        for run in runs
            for v in io.registered
                for i in 1:length(v)
                    t = v[i]
                    if !isdirect(t) && usize(t)>run # 1st run: usize above splitsize, 2nd run: usize>0
                        v[i] = hf(put(tmp,BToken(t,io.buffer)))
                    end
                end
            end
        end

        else

        end
        for v in registered
            for t in v
                sizet = usize(t)
                if !(isdirect(t) or sizet==0)
                    if io.locateOnPut

                    else

                    end
                end
            end
        end
        else
            # simple append
        end
        io.writeofs = ofs
        buf = tmp.buffer
    else
        # normal IOShared: keep preserved data
        count = (ofs - preservedofs) %UInt # size of reserved content before ofs
        buf = _string_n(limit)
        ptr = pointer(buf)
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
        io.writeofs = size %UInt32
    end
    io.shared = 0
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
function ensure_readable(io::IOShared, count::UInt32)
    fillup(io,count)
    usize32(io) >= count && return nothing
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

NOOP if io.ioread==false
"""
function fillup(io::IOShared, count::UInt32)
    if io.writeofs-io.readofs < count && io.ioread
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
        if count== ise
    end
    nothing
end



## searching (including Matcher support)
## TODO locate umbenennen in findfirst?
## TODO wir brauchen Suche nach byte, Token, Substring. 
## TODO wir sollten das auf SubString implementieren und IOShared auf SubString 'casten'



Base.match(r::Matcher, io::IOShared) = match(r,io.readofs,usize(io),io.buffer)

# TODO wir brauchen Suche ohne Matcher für Byte und String in matcher.jl
# TODO und hier und in Token Aufruf beider für IO

"""
locate a single byte in current buffer.

return typemax(UInt32) if not found or 1st offset found.
does NOT fillup!

"""
function _locate(pool::IOShared,pattern::UInt8, position::UInt32=0%UInt32) # TODO nicht besser absoluter offset??!
    
    size = usize(pool)%Int - position
    p = pool.ptr+pool.readofs+position
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

 

#= unfertig!!! 
function Base.readuntil(io::IOShared, delim::UInt8; keep::Bool=false)

    rel = 0 # relative offset
    while true
        fillup(io,rel+1)
        if io.readofs+rel >= writeofs
            error("Delimiter not found in data")
        end
        if codeunit(io.buffer)

    end
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
=#


## Base API



"""
if not io.ioread, write all unmarked current contents to data sink.

written content is removed from this.
no buffer reorganization, but flushing can increase
re-use of an unshared buffer
"""
function Base.flush(io::IOShared) 
    if io !== nothing && !io.ioread
        endofs = min(io.mark,io.writeofs)
        if  endofs > io.readofs
            sswrite(io.io,io.readofs,endofs-io.readofs, io.buffer)
            io.readofs = endofs
        end
    end
    nothing
end


"return a new IOShared with same (shared) contents, but no mark, default preferredlimit"
function Base.copy(b::IOShared) 
    b.io === nothing || error("cannot copy IOShared with attached source/sink")
    return IOShared(io.readofs,usize(io),io.buffer)
end


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


"write all available bytes in from's buffer (with fillup)"
function Base.write(to::IOShared, from::IOShared) 
    to===from && error("write(io,io) not allowed for io::IOShared ")
    sswrite(to,from.readofs,available(from),from.buffer)
end



# size related operations


function Base.eof(io::IOShared)
    fillup(io,1%Uint32)
    return io.readofs == io.writeofs
end


function Base.bytesavailable(io::IOShared) 
    ret = usize(io)%Int
    if io.ioread 
        ret += bytesavailable(io.io)
    end
end


## offset related operations

Base.position(io::IOShared) = io.readofs %Int


"""
    seek(io::IOShared, n::Integer)

Seek moves read position given by position(io) to a new offset.

Please take into account that read and write operations may move
content within the internal buffer, offsets are usually not stable.



Use seek within currently defined content, given by the offsets 
from position(io) up to position(io)+usize(io) directly before seek operation,
or use mark to "freeze" content above mark position, and seek 
relative to mark position. 

If mark is 0, all offsets are preserved except for explicit resize operations.
"""
function Base.seek(io::IOShared, n::Integer)
    # different from IOBuffer which projects on valid offsets, due to a REPL requirement
    (n < 0 || n > io.writeofs) && throw(ArgumentError("Attempt to seek outside IOShared boundaries."))
    io.readofs = n%UInt32
    return io
end


Base.seekstart(io::IOShared) = seek(io,0)


function Base.seekend(io::IOShared)
    io.readofs = io.writeofs
    return io
end


function Base.mark(io::IOShared)
    io.preserveOffsets && error("IOShared has preservedOffsets - mark is not supported")
    io.mark = io.readofs
end


Base.ismarked(io::IOShared) = io.mark != NOMARK


function Base.unmark(io::IOShared)
    io.preserveOffsets && error("IOShared has preservedOffsets - unmark is not supported")
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

Warning: take care if io.io is not nothing
"""
function Base.truncate(io::IOShared, n::Integer)
    n < 0 && throw(ArgumentError("truncate failed, n must be ≥ 0, got $n"))
    pos = UInt32(n) # does check for 32 bit overflow
    if pos >io.writeofs 
        ensure_writeable(io,pos-io.writeofs)
        for i in io.writeofs:pos
            write(io,0x00)
        end
    end
    io.writeofs = pos
    io.readofs>io.writeofs && (io.readofs = io.writeofs)
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
function Base.take!(io::IOShared)
    ss = SubString(io.readofs,usize(io), shared(io,io.writeofs))
    io.readofs = io.writeofs = 0
    return ss
end


@inline Base.ensureroom(io::IOBuffer, nshort::Int) = ensurewritable(io, UInt32(nshort))


@noinline function Base.close(io::IOShared)
    if io.io !== nothing
        flush(io)
        close(io.io)
        io.io = nothing
    end
    io.ioread = false
    io.writeofs = 0
    io.readofs = 0
    io.mark = typemax(UInt32)
    io.locateOnPut = false
    io.preserveOffsets = false
    io.limit = 0 # enforces reallocation on any attempt to use again, used also as flag for isopen
    io.buffer = EMPTYSTRING
    nothing
end

Base.isopen(io::IOShared) = io.limit !=0


"equivalent to read everything"
function Base.empty!(io::IOShared)
    flush(io)
    io.readofs = io.writeofs
    if io.ioread 
        close(io.io)
        io.io = nothing
        io.ioread = false
    end
    return io
end



function checkrange(offset::UInt32, size::UInt64, io::IOShared)
    checksize(offset+size,io.writeofs)
    checksize(io.readofs,offset)
end


# package internal API
function register(pool::IOShared, tv::Vector{HybridFly})

end