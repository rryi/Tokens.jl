"""
    locate(what, ofs::UInt32, size::UInt64, buffer::String)

offset-oriented high performance search function without matcher.
searches what in codeunits(buffer,ofs+1:ofs+size).

Special treatment of searches for single bytes and byte pairs.
longer searches use a bloom filter algorithm, an improved version
of the algorithm used in Base._searchindex (in Julia Release 1.4).

"""
function locate end


locate(what::Union{AbstractString,AbstractChar},ofs::UInt32, size::UInt64, buffer::String) = locate(BToken(what), ofs, size, where)

function locate(what::UInt8, ofs::UInt32, size::UInt64, buffer::String)
    @boundscheck checklimit(ofs+size,usize(buffer))
    p = pointer(buffer)
    q = GC.@preserve buffer ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p+ofs, b, size%Csize_t)
    return q == C_NULL ? typemax(UInt32) : UInt32(q-p)
end


"very simple hash function used for bloom filters in ExactMatcher and locate"
bloomhash(b::UInt8) = UInt64(1) << (b & 0x3f)

"maximum number of bits allowed to be set in bloom filter"
const BLOOMFILTERLIMIT = 48

function locate(what::BToken, ofs::UInt32, size::UInt64, buffer::String)
    @boundscheck checklimit(ofs+size,usize(buffer))
    sw = usize(what)
    size<sw && return typemax(UInt32)
    if sw<=2
        sw==1 && return @inbounds locate(byte(what,0),ofs,size,buffer)
        sw==0 && return ofs
        # we search for a byte pair. 
        b0 = byte(what,0)
        b1 = byte(what,1)
        o2 = 0%UInt32 # o2 is offset within content to search
        pb = pointer(buffer)+ofs
        while size-1-o2 > 0
            # byte search - higher order methods do not pay off by 2-byte-search
            q = GC.@preserve buffer ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), pb+o2, b0, (size-1-o2)%Csize_t)
            if q == C_NULL
                return typemax(UInt32) 
            end
            o2 = (q-pb)%UInt32
            if b1 == byte(buffer,ofs+o2+1)
                return ofs+o2
            end
        end
        return typemax(UInt32)
    end
    # standard case: at least 3 bytes to search, buffer size >= pattern size.
    # we use an improved version of Base._searchindex with bloom filter. see https://github.com/rryi/SearchBenchmarks.jl
    #
    # (1) initialization: copy/paste from Exactmatcher initialization
    skip = sw # bytes to skip if last byte matches, any other byte does not match
    last = codeunit(what,sw)
    bloom_mask = bloomhash(last)
    bloom_skip = sw # no. of bytes to skip if byte not in filter
    j = sw 
    if sw<=BLOOMFILTERLIMIT # just fillup bloom filter. No degeneration countermeasures necessary
        bloom_skip = sw
        while (j-=1)>=1  # j=n is already processed
            pj = codeunit(what,j)
            if pj == last && skip == sw
                skip = sw - j
            end
            bloom_mask |= bloomhash(pj)
        end
    else
        # long pattern: do not overfill bloom filter which would result in degeneration
        bloom_bits = 1 # no. of bits set in bloom filter if <= 32
        bloom_skip = 1 # we have to count upwards until no. of bytes to skip if byte not in filter
        while (j-=1)>=1  # j=n is already processed
            pj = codeunit(what,j)
            if pj == last && skip == sw
                skip = sw - j
            end
            if bloom_bits <= BLOOMFILTERLIMIT 
                # we continue setting bits in bloom filter. BLOOMFILTERLIMIT should be a guess for max(p(bloom_skip)*bloom_skip)
                hash = bloomhash(pj)
                if hash&bloom_mask == 0
                    if bloom_bits < BLOOMFILTERLIMIT
                        # put in bloom filter up to 32 bits
                        bloom_mask |= hash
                    end
                    bloom_bits += 1 # gets >=BLOOMFILTERLIMIT to terminate bloom filter filling
                end
                if bloom_bits <= BLOOMFILTERLIMIT
                    bloom_skip += 1
                end
            else
                # do not put more bits into bloom filter.
                if skip<sw
                    # we have finished adding hashes to bloom filter
                    # and we have determined the skip distance if matching last byte
                    # nothing remains to be done in preprocessing - stop work.
                    break
                end
            end
        end
    end
    skip -= 1 # because in main loop, 1 byte of skip is skipped separately
    #
    # (2) kernel: loop through buffer until found / end of data
    w = size-sw # we already checked for size>=sw
    # w<0 implies: not found 
    i = ofs + sw # index of last byte in matching candidate in search
    while i < size # i==size could be a match, is treated after this loop
        if codeunit(buffer,i) == last
            # check candidate
            j = 1
            while j < sw
                if codeunit(buffer,i-sw+j) != codeunit(what,j)
                    break
                end
                j += 1
                # match found?
                if j == sw
                    return i-n
                end
            end
            # no match: skip and test bloom
            i += skip
            if i>=size # main loop finished?
                break
            end
        end
        i += 1
        if bloom_mask & bloomhash(codeunit(buffer,i)) == 0
            i += bloomskip
        end
    end
    if i==size
        # test end match
        j = 1
        while j <= sw
            if codeunit(buffer,i-sw+j) != codeunit(what,j)
                break # not found
            end
            j += 1
        end
        if j > n # means: loop without break , we have a match at the very end
            return i-sw
        end 
    end
    # not found
    return typemax(UInt32)    
end


# TEST pair performance ohne C code (danach löschen)
function locate2(what::BToken, ofs::UInt32, size::UInt64, buffer::String)
    @boundscheck checklimit(ofs+size,usize(buffer))
    sw = usize(what)
    size<sw && return typemax(UInt32)
    if sw==2
        # we search for a byte pair. 
        b0 = byte(what,0)
        b1 = byte(what,1)
        o2 = 0%UInt32 # o2 is offset within content to search
        pb = pointer(buffer)+ofs
        pend = pb+size-1 # points to last byte in buffer
        p2 = pb
        GC.@preserve buffer while p2<pb
            if unsafe_load(p2) == b0
                if unsafe_load(p2+1) == b1
                    return ofs+(p2-pb)
                end
            end
            p2 += 1
        end # while
        return typemax(UInt32)
    end
    return locate(what,ofs,size,buffer)
end
