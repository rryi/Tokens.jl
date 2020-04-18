
## benchmarking of Tokens.locate
#
# Base.__searchindex(s::ByteArray, t::ByteArray, i::Integer)
#      versus
# locate(pool::IOShared,ofs::UInt32,size::UInt64,s::String)
#
# functions are replicated here, to build statistics







## replicated functions


mutable struct LocateStats
    compareLastByte :: Int
    compareOtherByte :: Int
    bloomTest :: Int
    skips :: Int
end
LocateStats()=LocateStats(0,0,0,0)


function Base._searchindex(s::ByteArray, t::ByteArray, i::Integer, stats::LocateStats)
    n = sizeof(t)
    m = sizeof(s)

    if n == 0
        return 1 <= i <= m+1 ? max(1, i) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return something(findnext(isequal(_nthbyte(t,1)), s, i), 0)
    end

    w = m - n
    if w < 0 || i - 1 > w
        return 0
    end

    bloom_mask = UInt64(0)
    skip = n - 1
    tlast = _nthbyte(t,n)
    for j in 1:n
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast && j < n
            skip = n - j - 1
        end
    end

    i -= 1
    while i <= w
        stats.compareLastByte += 1
        if _nthbyte(s,i+n) == tlast
            # check candidate
            j = 0
            while j < n - 1
                stats.compareOtherByte += 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i+1
            end

            # no match, try to rule out the next character
            stats.bloomTest += 1 ## ignore i==w case
            if i < w && bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                i += n
                stats.skips += n
            else
                i += skip
                stats.skips += skip
            end
        elseif i < w
            stats.bloomTest += 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                stats.skips += n
                i += n
            end
        end
        i += 1
        stats.skips += 1
    end
    0
end




minihash(v:UInt8) = (1%UInt64)<<(v&63)


"""
    locate(pool::IOShared,ofs::UInt32,size::UInt64,s::String)

Search in pool for a byte sequence of size bytes, beginning at
offset ofs (0-based!) in String s.

Return the offset (0-based) in pool.buffer or typemax(UInt32) (not found).

TODO use boyer-moore search if size and usize(pool) are 'large'.
"""
function locate(pool::IOShared,ofs::UInt32,size::UInt64,s::String, stats::LocateStats)
    if size == 0
        return pool.readofs
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
            stats.compareLastByte += 1
            if cp==cs_last
                ## last byte matches. test successive backwards up to i-size+1
                j = size-1
                p0 = p_ptr+i-j # pointer to offset 0 in current match
                while j-=1 >= 0
                    cp = unsafe_load(p0+j)
                    stats.compareOtherByte += 1
                    if cp != unsafe_load(s_ptr+j)
                        break
                    end
                end
                if j<0
                    #match
                    return i-(size-1) # offset of 1st byte
                end
                # no match.
                # if j>skip #TODO try to enlarge skip by hash test
                i += skip
                stats.skips += skip

            elseif (stats.bloomTest += 1;bloom & minihash(cp) == 0)
                # cp does not occur in s
                i += size
                stats.skips += size

            elseif
                # default skip: 1 byte
                i += 1
                stats.skips += 1
            end
        end
    end
end


function buildStats()
    p = rand(0x20:0x7f,10000)
    s = rand(0x40:0x7f,1000)
    bp = IOShared(10000) # pool to search in
    bs = IOShared(1000) # search string
    write(bp,p)
    write(bs,s)
    lsSearch = LocateStats()
    lsLocate = LocateStats()
    for j in 3:30 # search size
        lsSearch = LocateStats()
        lsLocate = LocateStats()
        for i in 4000:5000 # scan size
            bs.readofs = (i*j*991)&511
            bs.writeofs = bs.readofs+j
            bp.readofs = (i*j*3373)&4095
            bp.writeofs = bp.readofs+i
            foundL = locate(bp,bs.readofs,i,lsLocate) + 1%UInt32
            as = s[bs.readofs+1:bs.writeofs]
            ap = p[bp.readofs+1:bp.writeofs]
            foundS _searchindex(ap,as,1,lsSearch)
            if foundL!=foundS
                println("mismatch: i=$i j=$j ",foundL, foundS)
                println("search: $bs")
                println("in pool: $bp")
            end
        end
        println()
        println("Stats zu j=$j")
        println("stats zu locate")
        show(lsLocate)
        println("Stats zu _search")
        show(lsSearch)
    end
end

buildStats()
