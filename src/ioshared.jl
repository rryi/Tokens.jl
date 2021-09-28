# This file is derived from julia iobuffer.jl. License is MIT: https://julialang.org/license

## work with String as UInt8 buffer via I/O primitives and share with tokens/substrings ##

# Stateful string

const MaybeIO = Union{Nothing, IO}

"a constant for a default allocation limit in IOShared and other resizable array structures"
const DEFAULTLIMIT = 1024%UInt32

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

 
 All operating flavours maintain two internal offsets, marking the currently 
 used part of the internal buffer.
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
 

In addition to the operating mode, every IOShared can act as a *heap for
flyweight tokens:* vectors of flyweight tokens can be registered to the 
IOShared, using it for all content which needs a buffer. 
[`Tokenvector`](@ref) is based on this feature. Compared to Vector{Token},
the overhead per token is halved, and memory locality is 
improved for operations on the vector. Heap space is allocated from the end
of the internal buffer, reducing the *limit* offset. This way, it does not
conflict with the operating modes for reading/writing. 

IOShared has an internal garbage collection for its heap: if it runs out 
of free memory in its buffer, it allocates a new buffer, and copies
living heap entries to a contiguous memory block. If no other objects 
keep a reference to the old buffer, it gets (julia) garbage collected. 
Sharing content with SubString, HToken and BToken will prevent garbage 
collection of the old buffer. This can have 
effects similar to memory leaks in classical C code: a small SubString/Token
blocks a possibly large buffer. For excellent memory efficiency, avoid
sharing only small parts of an IOShared with SubString/HToken/BToken
structures. With TokenVector{VToken} and VToken elements, you can leave all
content in one IOShared buffer, and avoid most content copying. 
A typical use case is a parse process, which keeps only a small percentage of 
source text in tokens. 

"""
mutable struct IOShared <: IO
    buffer :: String # PRIVATE! memory with byte data.
    readofs :: UInt32 # read position offset / number of consumed bytes
    writeofs :: UInt32 # write (append) position offset / end of defined data
    shared :: UInt32 # offset in buffer behind last shared byte (0: nothing shared)
    mark :: UInt32 # offset of data not to flush/forget. typemax: no active mark
    limit :: UInt32 # buffer limit for read/write operations, memory above is used by put function
    preferredlimit :: UInt32 # limit in case of reallocation due to sharing
    registered :: Vector{HybridFly} # true: mark is always 0 (offsets are guaranteed not to change)
    io::MaybeIO # nothing or if ioread: source for fillup, else target for flush
    eolRemoved :: Int32 # <0: undefined. else: no. of eol bytes which were removed from buffer by reorg-s
    eol :: UInt8 #  last byte of an end-of-line-sequence (default: 10, change to 13 for Mac text files)
    ioread :: Bool # true: io is source for fillup. false: io is target for flush or nothing
    compressOnPut :: Bool # true: put does lookup content, share instead of append if found
    function IOShared(io::MaybeIO,ioread::Bool,limit::UInt32) 
        ioread && io===nothing && error("ioread must be false if io is nothing")
        z = zero(UInt32)
        s = _string_n(limit)
        new(s,z,z,z,NOMARK,limit,limit,Vector{HybridFly}(),io,-1,10,ioread,false)
    end
    function IOShared(offset::UInt32, size::UInt64, s::String)
        @boundscheck checkrange(offset,size,s)
        size32= UInt32(size) # check on overflow necessary: cannot handle too long strings
        preferredlimit = max(size32,DEFAULTLIMIT) # well ... just a very simple guess
        limit = UInt32(ncodeunits(s)) # check on overflow necessary: cannot handle too long strings
        new(s,offset,offset+size32,offset+size32,NOMARK,limit,preferredlimit,Vector{HybridFly}(),nothing,-1,10,ioread,false)
    end
end

IOShared(io::MaybeIO,ioread::Bool) = IOShared(io,ioread,DEFAULTLIMIT)

IOShared(limit::UInt32=DEFAULTLIMIT) = IOShared(nothing,false,limit)

IOShared(s::SubString{String}) = IOShared(UInt32(s.offset),s.ncodeunits%UInt64,s.string)

IOShared(s::String) = IOShared(zero(UInt32),usize(s),s)

IOShared(t::BToken) = IOShared(t.offset,t.len,t.buffer)

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

Of course, the returned String must be treated immutable - writing to it 
via pointer operations may result in system crashes.

_share is regarded a private function. To obtain the text content of an IOShared,
use read operations or  [`get`](@ref) .
"""
function _share(io::IOShared,share::UInt32)
    if (io.shared<share)
        io.shared = share
    end
    return io.buffer
end


"share the whole internal buffer. ATTENTION: may contain uninitialized data"
_share(io::IOShared) = _share(io,usize32(io.buffer))


"currently loaded content size in bytes. Changes e.g. on fill, flush, read, write"
usize(io::IOShared) = usize32(io) % UInt64


usize32(io::IOShared) = io.writeofs-io.readofs





"""
    unsafe_put(heap::IOShared, t::Token{F})


put contents of t into the heap of an IOShared and return a [`FlyToken`](@ref) referencing
this IOShared with the same content.

Is marked unsafe, because the following preconditions are not checked (task of caller):

 * enough free space in heap
 * token is not direct
 * token size > 0

Method is used internally when storing in a registered [`TokenVector`](@ref),
and by realloc.

Only module-intern use, from outside use [`put`](@ref), which ensures preconditions.

If heap.compressOnPut is true, a search for the contents in heap is performed,
found contents is referenced. This can reduce memory consumption 
but increases CPU use. Default is false, change it with compressOnPut(heap,true)
"""
function unsafe_put(heap::IOShared, t::Token{F}) where F
    size = usize32(t)
    if heap.compressOnPut 
        # try to find in in heap space
        ofs = locate(t,heap.limit,usize(heap.buffer)-heap.limit,heap.buffer)
        if ofs<typemax(UInt32)
            return (t.fly & !OFFSET_BITS) + ofs
        end
    end
    # not found: append data in heap
    # ensure_writeable(heap,size) # already done by caller
    heap.limit -= size
    ofs = heap.limit
    b = heap.buffer
    bt = t.buffer
    GC.@preserve b bt unsafe_copyto!(pointer(b)+ofs, pointer(bt)+offset(t), size)
    return (t.fly & !OFFSET_BITS) + ofs
end




"""
    put(heap::IOShared, s, ::Token{F})

return a [`Token`](@ref) with a string representation of s. Returned token is
direct or uses heap as its backing buffer for its content.

s can be anything for which a Token{F} constructor exists, 
in particular AbstractString, AbstractToken, Real, Integer, Bool.
If s isa AbstractToken, its category is preserved, 
for any other type of s, a category derived from the type of s is used,
e. g. T_INT for integers or T_TEXT for strings.
    
put stores contents in a way not interfering with any read and write access on heap. 
The IOShared parameter is named heap to indicate how it works: put uses a "heap area" 
in the internal buffer at the physical end of the buffer, growing towards
lower adresses. heap area is share-protected, not available for read and write access,
and preserved on a buffer reallocation. 

If s has its content already in heap (s is a token or substring using heap-s buffer) 
or returned token is direct, heap is not changed.

If heap.compressOnPut is true, a search for the content is performed,
found content is referenced, avoiding content move and memory consumption.
The  price is increased CPU use: put is not O(1) but O(total used buffer size),
in this case. Default is false, change it with compressOnPut(heap,true)
"""
put(heap::IOShared, s, ::Type{Token{F}}) where F = put(heap,Token{F}(s))

function put(heap::IOShared, t::Token{F}) where F
    size = usize32(t)
    if !isdirect(t) && size>0 && t.buffer !== heap.buffer
        if F <: HybridFly && size<=MAX_DIRECT_SIZE
            return HToken(DirectFly(t))
        end
        #= search here makes no sense, is performed by unsafe_put iff heap.compressOnPut is true
        # already in heap?
        ofs = locate(t,heap.limit,usize(heap.buffer),heap.buffer)
        if ofs<typemax(UInt32)
            return Token{F}((t.fly & !OFFSET_BITS) + ofs,heap.buffer)
        end
        # or in content area?
        ofs = locate(t,0,heap.writeofs,heap.buffer)
        if ofs<typemax(UInt32)
            return Token{F}((t.fly & !OFFSET_BITS) + ofs,_share(heap,ofs+size))
        end
        # not found
        =#
        ensure_writeable(heap,size)
        f = unsafe_put(heap, t)
        t = Token{F}(f,heap.buffer) # we must not use share: all above offset heap.limit is implicitly shared
    end
    return t
end



"set heap.compressOnPut for subsequent put calls"
function compressOnPut(heap::IOShared, compress::Bool) 
    heap.compressOnPut = compress
    return nothing
end


"compute tuple () no. of lines before offset ofs, offset of begin of line containing position ofs)"
function _lineofs(io::IOShared, ofs::UInt32) :: Tuple{UInt32,UInt32}
    lines = zero(UInt32)
    ofs = 0%UInt32 # position of begin of line 
    while (oeol=locate(io.eol,ofs,(io.readofs-ofs)%UInt64,io.buffer))<io.readofs
        ofs = oeol+one(UInt32)
        lines += one(UInt32)
    end
    return (lines, ofs)
end


"""
    _eolAdjust(io::IOShared, preservedOfs::UInt32) ::UInt32

called if a reorg function wants to remove data section 0..preservedOfs,
before content destruction.

if eol processing is not activated (default), return preservedOfs

Otherwise, counts all EOL markers in buffer up to offset preservedOfs and
add io to eolRemoved. Return the offset of the begin of the last text line
to be removed. This will enforce that buffer contains the whole line, e.g.
to generate line listings in error situations.
"""
function _eolAdjust(io::IOShared, preservedOfs::UInt32)
    if io.eolRemoved>=0
        
    end
    return preservedOfs
end




"""
    linepos(io::IOShared) :: Pair{Int,Int}

return -1 => -1 (undefined) or lineNo =>index of the current read position in line.
lineNo and index are 1-based (first byte of a file has (1,1) as its linepos)

text line structure is undefinded by default. 
To enable line number tracking, call [`tracklines`](@ref).

linepos is not cheap: it scans for end-of-line markers throughout the
internal buffer, which is quite expensive for large buffers. It is
intended for error reporting in parsers.

Consider reading complete lines and counting lines in application code,
if you frequently need line number information.

"""
function linepos(io::IOShared) :: Pair{Int,Int} 
    io.eolRemoved<0 && return -1 => -1
    lines, ofs = _linepos(io,io.readofs)
    return lines+io.eolRemoved+1 => io.readofs - ofs + 1
end

"""
    tracklines(io::IOShared, eol::UInt8, linesRemoved::Int)

activate line number tracking for *io*, using *eol* as marker byte for an end-of-line sequence
and initialize the number of already removed lines with *linesRemoved*. Call it immediately 
after constructing io with *linesRemoved*==0.

Line number tracking has its price: it requires scanning for end-of-line markers in
possibly huge text buffers. Therefore, it is disabled by default.
"""
function tracklines(io::IOShared, eol::UInt8, linesRemoved::Int)
    io.eol = eol
    io.linesRemoved = linesRemoved
    return nothing
end


"""
move memory within an IOShared buffer: insert/delete at ofs

Method is regarded private and intended to be used within IOShared, only.
External API is function resize.

ofs is internal offset into io-s buffer.
PRECOND: limit large enough, no share at ofs, no delete beyond io.writeofs
ofs <= io.writeofs, if resize<0: ofs-resize <= io.writeofs

readofs, writeofs and mark are adjusted in io. 
No reallocation - if resize would violate sharing, an error is thrown.

Resize of IOShared having registered tokens can fail and is not recommended!

Resize returs true on a sucessful operation, else false.
"""
function _resize(io::IOShared, ofs::UInt32, resize::Int)
    @boundscheck begin
        (io.shared <= ofs # never chance shared contents
        && ofs <= io.writeofs # not beyond end of content
        && ofs <= io.writeofs + resize  # no delete beyond end of content (resize<0)
        && io.writeofs+resize <= io.limit # enough space (relevant if resize>0)
        ) || error("preconditions violated for resize by $resize at $ofs in $io")
    end
    #= NO!! unnecessary!! all registered tokens are share-protected!!
    if ofs<io.writeofs
        # we need to adjust registered tokens referencing nonheap buffer memory
        for v in io.registered
            for i in 1:length(v)
                t = v[i]
                s = t.len
                if !isdirect(t) && s>0 
                    # we have a heap based content, test for need to relocate it
                    o = offset(t)
                    if o >= ofs
                        # adjust offset if necessary
                        if o < io.writeofs
                            v[i] += resize
                        end
                    else
                        if o+s>ofs
                            # not nice - token would be destroyed and must be relocated
                            if io.writeofs+s>io.limit
                                return false  # we give up - not enough space to safely relocate heap token.
                            end
                            v[i] = hf(unsafe_put(io,BToken(bf(t),io.buffer))) 
                        end
                    end
                end
            end
        end
    end
    =#
    s = io.buffer
    ptr = pointer(s)
    GC.@preserve s begin
        if resize>0 #insert is the most frequently used case
            ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                  ptr+ofs+resize, ptr+ofs, io.writeofs-ofs)
            
        else #delete/noop
            resize == 0 && return nothing
            ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                  ptr+ofs, ptr+ofs-resize, io.writeofs-ofs+resize)
        end
    end
    io.writeofs += resize
    ofs<io.readofs && (io.readofs += resize)
    io.mark != NOMARK && ofs<io.mark && (io.mark += resize)
    return nothing
end



"""
allocate a new buffer and preserve content, resizing at offset ofs.

Method tries to flush first, to reduce allocation as well as
the amount of bytes to copy.

io is changed, adjusted ofs is returned.

Inserted bytes (resize>0) are uninitialized.

ofs precondition: io.readOfs <= ofs <= io.writeOfs

realloc is used internally, external use is 
not recommended.

"""
function _realloc!(io::IOShared, ofs::UInt32, resize::Int) ::UInt32
    flush(io) # write to sink to decrease used area
    preservedOfs = min(io.readofs, io.mark) # begin of protected content which must be copied. end is io.writeofs
    if io.eolRemoved >= 0
        # we have EOL tracking. reduce preservedOfs such that is the begin of a text line.
        # update io.eolRemoved with number of lines below preservedOfs 
        (lines, preservedOfs) = _lineofs(io,preservedOfs)
        io.eolRemoved += lines
    end
    need = io.writeofs - preservedOfs + resize # minimum size of new buffer (without heap).
    @boundscheck begin
        (io.readofs <= ofs 
        && ofs <= io.writeofs
        && need >= 0 && need <= typemax(UInt32)
        && (ofs-resize <= io.writeofs)
        ) || boundserror(io,ofs,resize)
    end
    # compute size of all tokens which must be put into heap because content is not preserved
    relocSize = 0 # size of all tokens not already in heap
    heapUsed = 0 # size of tokens already in heap
    oldLimit = io.limit
    for v in io.registered
        for t in v
            s = t.len
            o = offset(t)
            if !isdirect(t) 
                if o<oldLimit
                    # we will relocate all registered tokens not already in heap
                    # reason: we want shared==0 on return.
                    relocSize += s
                else
                    heapUsed += s
                end
            end
        end
    end
    heapSize = usize(io.buffer)-oldLimit
    need += relocSize + min(heapSize,heapUsed) # keep old heap if already compressed
    #
    # new limit: several considerations.
    # (1) at least, it must be >= need
    # (2) should not be smaller than preferredlimit
    # (3) ensure that write(UInt8) is O(1) ==> enlarge need such that free space is O(need)
    newBuffersize = max(io.preferredlimit,UInt32((need*3)>>1)) # 50% more than need
    oldbuffer = io.buffer
    newbuffer = _string_n(newBuffersize) 
    io.buffer = newbuffer
    io.limit = newBuffersize
    keptHeapOffset = typemax(UInt32) # all offsets above are in old heap and only to adjust
    keptHeapDelta = 0
    GC.@preserve oldbuffer newbuffer begin # for safety. might be unnecessary
        if heapSize<heapUsed
            # keep old heap as contiguous memory block. 
            # move it now, so that relocation can search in new heap.
            ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
              pointer(newBuffer)+newBuffersize-heapSize, pointer(oldBuffer)+oldLimit, heapSize)
            io.limit -= heapSize
            keptHeapOffset = oldLimit
            keptHeapDelta = io.limit-oldLimit
        end
        # treatment of registered tokens: adjust offsets, maybe move content
        for v in io.registered
            for i in 1:length(v)
                t = v[i]
                if !isdirect(t) && t.len>0
                    o = offset(t)
                    if o >= keptHeapOffset
                        v[i] = t+keptHeapDelta
                    else
                        v[i] = hf(unsafe_put(io,BToken(t,oldbuffer)))
                    end
                end
            end
        end
        src = pointer(oldbuffer)
        newbuffer = io.buffer
        dst = pointer(newbuffer)
        count = (ofs - preservedOfs) %UInt # size of reserved content before ofs
        ## copy first part: preservedOfs..ofs
        ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
            dst, src+preservedOfs, count)
        # copy 2nd part: ofs..io.writeofs
        if resize<0
            count2 =  (io.writeofs-ofs+resize) %UInt
            if count2>0
                ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                    dst+count, src+ofs-resize, count2)
            end
        else
            count2 =  (io.writeofs-ofs) %UInt
            if count2>0
                ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                    dst+count+resize, src+ofs, count2)
            end
        end
        ofs -= preservedOfs
        io.readofs -= preservedOfs
        io.mark != NOMARK && (io.mark -= preservedOfs)
        io.writeofs += resize-preservedOfs
    end
    io.shared = 0
    return ofs
end


"""
    shrink!(io:IOShared, reserve::UInt32, unshare::Bool = true)

if unused memory of io exceeds reserve, reallocate internal buffer of io such that exactly reserve bytes are unused by content or heap. 

If unshare is true, all registered flyweight tokens are moved to heap, which can increase used memory. But it ensures
that content area is unshared on return.

No memory optimization is performed - that should be done before.
"""
function shrink!(io::IOShared, reserve::UInt32, unshare::Bool = true)
    if io.limit - io.writeofs > reserved
        if unshare
            oldLimit = io.limit
            for v in io.registered
                for t in v
                    s = t.len
                    o = offset(t)
                    if !isdirect(t) 
                        if o<oldLimit
                            # we will relocate all registered tokens not already in heap
                            # reason: we want shared==0 on return.
                            put(io,BToken(t,io.buffer))
                        end
                    end
                end
            end
            io.shared = 0
        end    
        # compute size of all tokens which must be put into heap because content is not preserved
        if io.limit - io.writeofs > reserved
            toshrink = io.limit - io.writeofs - reserved
            heapsize = usize32(io.buffer)-io.limit
            newBuffersize = usize32(io.buffer)-toshrink
            oldbuffer = io.buffer
            newbuffer = _string_n(newBuffersize) 
            io.buffer = newbuffer
            io.limit -= toshrink
            GC.@preserve oldbuffer newbuffer begin # for safety. might be unnecessary
                # keep old heap as contiguous memory block. 
                ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                pointer(newBuffer)+newBuffersize-heapSize, pointer(oldBuffer)+oldLimit, heapSize)
                # keep content area as is
                ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt),
                    newbuffer, oldbuffer, io.writeofs)
            end
        end
    end
    return nothing
end

"""

    modify!(io::IOShared, ofs::UInt32, resize::Int)

Ensure that io.buffer is writeable starting at ofs, and resize current content at ofs.

If resize is negative, deletion is restricted to the current content.
If io.shared>ofs, or if io.limit < ofs+resize, a new buffer is allocated.

Buffer allocation may change offsets, i.e. io.readofs, io.writeofs and ofs.
Instance variables are adjusted, the adjusted value of ofs is returned by modify.

In case of an insert/append, content at ofs..ofs+resize is uninitialized.

Method is used package-internally, but also exported as central low level resize method.

Common use is appending, i.e. ofs==io.writeofs, resize>0.
Any other use makes things complicated if a data sink or data source is assigned to io.
"""
function modify!(io::IOShared, ofs::UInt32, resize::Int)
    if io.shared<=ofs
        # good chances for a noop
        if io.writeofs+resize<=io.limit
            # insert/delete
            if ofs==io.writeofs && resize >=0
                # normal append: expected to be the most frequent case
                io.writeOfs += resize
                return ofs
            end
            _resize(io,ofs,resize)
            return ofs
        end
        # not enough space at end - can we reuse space at beginning?
        if io.eolRemoved<0 
            # we require no eol tracking, because it does not allow resize work with 0 < resize pos < readofs
            flush(io)
            preservedOfs = min(io.readofs,io.mark)
            delta = io.shared%Int - preservedOfs # bytes deleteable at shared if <0
            if io.writeofs+resize+delta <= io.limit
                # removing obsolete content after shared is enough
                _resize(io,io.shared,delta) # remove free space at beginning
                ofs += delta
                _resize(io,ofs,resize)
                return ofs
            end
        end
    end
    # nothing but realloc helps ...
    return _realloc(io,ofs,resize)
end


function ensure_writeable(io::IOShared, count::UInt32)
    if io.shared>io.writeofs || io.writeofs+count>io.limit
        _realloc(io,io.writeofs,count%Int)
        io.writeofs -= count # we only ensured space for writing. write operation will change offset
    end
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
        ptr = pointer(s)
        GC.@preserve s unsafe_read(io.io,ptr+io.writeofs,count%UInt)
        io.writeofs += count
        # TODO: check if we have reached EOF and close io in that case?
    end
    nothing
end



locate(what::Union{AbstractString,AbstractChar},io::IOShared) = locate(BToken(what), io.readofs,usize(io),io.buffer)

locate(what::UInt8,io::IOShared) = locate(what, io.readofs,usize(io),io.buffer)



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
    buffer = pool.buffer
    p_ptr = pointer(buffer)+pool.readofs
    p_size = usize(pool)
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
    q = GC.@preserve buf ccall(:memchr,Ptr{UInt8},(Ptr{UInt8},Int32,Csize_t),pointer(buf)+io.readofs,delim,usize(io))
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

Read a Token in serialized format, sharing the buffer (no content copying).
This is a main benefit of IOShared over IOBuffer: 
IOShared is still mutable, it keeps track of shared portions 
and reallocates if shared portions have to be changed.

"""
function Base.read(io::IOShared, ::Type{Token{T}}) where T
    p = read(io,Packed31)
    size = t.len
    ensure_readable(io,size)
    if T <: HToken && size <= MAX_DIRECT_SIZE
        return HToken(read(io,p,DirectFly))
    end
    t = BufferFly(p) + io.readofs
    io.readofs += size
    @inbounds return Token{T}(t,_share(io,io.readofs))
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
        ptr::Ptr{T} = pointer(b)+from.readofs
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
    GC.@preserve b ret = unsafe_load(pointer(b)+from.readofs)
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
    GC.@preserve b unsafe_copyto!(pointer(b)+to.writeofs, p, nb)
    to.writeofs += nb
    return nothing
end


function Base.write(to::IOShared, byte::UInt8)
    ensure_writeable(to,1)
    b = to.buffer
    GC.@preserve b unsafe_store!(pointer(b)+to.writeofs, byte)
    to.writeofs += 1
    return nothing
end



"write all available bytes (no waiting for async streams)"
function Base.write(to::IOShared, from::IO)
    # would work, but allocates intermediate Vector #  = write(to,read(from))
    b = to.buffer
    size = UInt32(bytesavailable(from))
    ensure_writeable(to,size)
    GC.@preserve b unsafe_read(from,pointer(b)+to.writeofs,size)
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

Warning: take care if io.io is not nothing
"""
function Base.truncate(io::IOShared, n::Integer)
    n < 0 && error("truncate failed, n must be ≥ 0, got $n")
    n<io.writeofs && length(io.registered)>0 && error("truncate not implemented for IOShared used as heap. ")
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


"peek a byte at offset (aka 0-based index) in data of this"
function byte(s::IOShared, ofs::UInt32)
    ensure_readable(s,ofs+1)
    b = GC.@preserve s.buffer unsafe_load(pointer(s.buffer)+s.readofs+ofs)
    return b
end


"lookahead 1 byte"
Base.@propagate_inbounds function Base.peek(from::IOShared)
    ensure_readable(from,1)
    b = from.buffer
    GC.@preserve b begin
        ptr = pointer(b)+from.readofs
        x = unsafe_load(ptr)
    end
    return x
end


"lookahead n bytes, returned as a BToken"
function Base.peek(io::IOShared, size::UInt64)
    ensure_readable(io,size)
    # reference string data in IOShared instance
    @inbounds return BToken(io.readofs,size,_share(io,io.readofs+size))
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
    io.compressOnPut = false
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


