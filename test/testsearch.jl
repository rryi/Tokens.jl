# include("C:\\Users\\RR\\.julia\\dev\\Tokens\\test\\testsearch.jl")

# comparing _searchindex from base.search.jl with variants
# Author Robert Rudolph www.r2c.de
import Base.ByteArray, Base._nthbyte, Base._search_bloom_mask


@enum StatsFields SFtime=1 SFloops SFbloomtests SFbloomskips SFbloombits

function bitcount(bitset::UInt64)
    count = 0
    while  bitset>0
        count += bitset&(1%UInt64)
        bitset = bitset >>>1
    end
    count
end

struct Stats
    dict:: IdDict{Function,Array{Int,2}}
    function Stats(size)
        d = IdDict(zeros=>zeros(Int,Int(size),Int(typemax(StatsFields))))
        new(d)
    end
end

function Base.print(io::IO,s::Stats)
    for p in s.dict
        for j in Int(typemin(StatsFields)):Int(typemax(StatsFields))
            print(io,string(p.first),",",StatsFields(j))
            for v in p.second[:,j]
                print(io,", ",v)
            end
            println(io)
        end
    end
end

const nullvec = zeros(Int,2)

function record(stats::Stats, f::Function, i::Int, sv::Vector)
    vec = get(stats.dict,f,nullvec)
    if vec===nullvec
        vec = copy(get(stats.dict,zeros,nullvec))
        push!(stats.dict,f=>vec)
    end
    vec[i,:] = sv
end


function _searchindex(s::String, t::String, i::Integer)
    # Check for fast case of a single byte
    lastindex(t) == 1 && return something(findnext(isequal(t[1]), s, i), 0)
    _searchindex(unsafe_wrap(Vector{UInt8},s), unsafe_wrap(Vector{UInt8},t), i)
end

function _searchindex_julia(s::ByteArray, t::ByteArray, i::Integer,sv::Vector)
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
    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i <= w
        loops += 1
        if _nthbyte(s,i+n) == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i+1 # no stats update
            end

            # no match, try to rule out the next character
            bloomtests += 1
            if i < w && bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                bloomskips += 1
                i += n
            else
                i += skip
            end
        elseif i < w
            bloomtests += 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                bloomskips += 1
                i += n
            end
        end
        i += 1
    end
    sv[Int(SFloops)] = loops
    sv[Int(SFbloomtests)] = bloomtests
    sv[Int(SFbloomskips)] = bloomskips
    sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end

"""
variant: bloomfilter without last byte of pattern, apply
filter on tested byte (instead following byte)
"""
function _searchindex_v1(s::ByteArray, t::ByteArray, i::Integer,sv::Vector)
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
    skip = n # -1 removed because unconditional i +=1 at end of main loop removed
    tlast = _nthbyte(t,n)
    for j in 1:n-1 # n-1: exclude last byte of pattern from bloom filter
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast
            skip = n - j # -1 removed because unconditional i +=1 at end of main loop removed
        end
    end

    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i <= w
        loops += 1
        silast =  _nthbyte(s,i+n)
        if silast == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i+1
            end

            # no match, use skip distance
            # original code does bloom test assuming skip==1 here
            # better postpone that to next ineration
            i += skip
        elseif bloom_mask & _search_bloom_mask(silast) == 0
            bloomtests += 1
            bloomskips += 1
            i += n
        else # worst case: minimal skip
            bloomtests += 1
            i += 1
        end
    end
    sv[Int(SFloops)] = loops
    sv[Int(SFbloomtests)] = bloomtests
    sv[Int(SFbloomskips)] = bloomskips
    sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end



"""
variant: test bloomfilter first
"""
function _searchindex_v2(s::ByteArray, t::ByteArray, i::Integer,sv::Vector)
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

    skip = n # -1 removed because unconditional i +=1 at end of main loop removed
    tlast = _nthbyte(t,n)
    bloom_mask = UInt64(_search_bloom_mask(tlast))
    for j in 1:n-1 # n-1: last byte of pattern is already in bloom filter
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast
            skip = n - j # -1 removed because unconditional i +=1 at end of main loop removed
        end
    end
    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i <= w
        loops += 1
        silast =  _nthbyte(s,i+n)
        bloomtests += 1
        if bloom_mask & _search_bloom_mask(silast) == 0
            i += n
            bloomskips += 1
        elseif silast == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
                # match found?
                if j == n - 1
                    return i+1
                end
            end
            # no match, use skip distance
            # original code does bloom test assuming skip==1 here
            # better postpone that to next ineration
            i += skip
        else
            i += 1
        end
    end
    sv[Int(SFloops)] = loops
    sv[Int(SFbloomtests)] = bloomtests
    sv[Int(SFbloomskips)] = bloomskips
    sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end

const optimal_bloom_size = 160 # best guess: max of p(not in bloom)*size(bloom

"""
variant: like v2 but avoid bloom filter degeneration on large pattern sizes
theory says: optimal size is when clearbits=32 (maximal expected skip size).
practically, bloom size about 160 is fastest - WHY???
"""
function _searchindex_v3(s::ByteArray, t::ByteArray, i::Integer,sv::Vector)
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

    skip = n # -1 removed because unconditional i +=1 at end of main loop removed
    tlast = _nthbyte(t,n)
    bloom_mask = UInt64(_search_bloom_mask(tlast))
    bloom_size = 1
    clearbits = 63 # no of clear bits in bloom
    j = n
    while (j-=1)>=1 # n-1: last byte of pattern is already in bloom filter
        # if (clearbits-1)*(bloom_size+1)>clearbits*bloom_size
        if clearbits>=bloom_size # slightly weakened
            b = _nthbyte(t,j)&63
            hash =  (1%UInt64)<<b
            clearbits -= (hash& ~bloom_mask)>>> (b)
            bloom_mask |= hash
            bloom_size += 1
        end
        if _nthbyte(t,j) == tlast && skip>n-j
            skip = n - j # -1 removed because unconditional i +=1 at end of main loop removed
        end
    end
    #println("patternsize=",n,", bloom_size=",bloom_size,", clearbits=",clearbits)

    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i <= w
        loops += 1
        silast =  _nthbyte(s,i+n)
        bloomtests += 1
        if bloom_mask & _search_bloom_mask(silast) == 0
            i += bloom_size
            bloomskips += 1
        elseif silast == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i+1
            end

            # no match, use skip distance
            # original code does bloom test assuming skip==1 here
            # better postpone that to next ineration
            i += skip
        else # worst case: minimal skip
            i += 1
        end
    end
    sv[Int(SFloops)] = loops
    sv[Int(SFbloomtests)] = bloomtests
    sv[Int(SFbloomskips)] = bloomskips
    sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end


"""
variant: like v3 but 2 bloom filter
"""
function _searchindex_v4(s::ByteArray, t::ByteArray, i::Integer,sv::Vector)
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

    skip = n # -1 removed because unconditional i +=1 at end of main loop removed
    tlast = _nthbyte(t,n)
    bloom_mask = UInt64(_search_bloom_mask(tlast))
    bloom_half = UInt64(_search_bloom_mask(tlast))
    bloom_size = 1
    for j in 1:n-1 # n-1: last byte of pattern is already in bloom filter
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if (j>n/2)
            bloom_half |= _search_bloom_mask(_nthbyte(t,j))
            bloom_size += 1
        end
        if _nthbyte(t,j) == tlast
            skip = n - j # -1 removed because unconditional i +=1 at end of main loop removed
        end
    end

    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i <= w
        loops += 1
        silast =  _nthbyte(s,i+n)
        hash = _search_bloom_mask(silast)
        bloomtests += 1
        if bloom_mask & hash == 0
            i += bloom_size
            bloomskips += 1
        elseif silast == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i+1
            end

            # no match, use skip distance
            # original code does bloom test assuming skip==1 here
            # better postpone that to next ineration
            i += skip
        else # worst case: minimal skip, but try half bloom
            i += 1
            bloomtests += 1
            if bloom_half & hash == 0
                i +=bloom_size-1
                bloomskips += 1
            end
        end
    end
    sv[Int(SFloops)] = loops
    sv[Int(SFbloomtests)] = bloomtests
    sv[Int(SFbloomskips)] = bloomskips
    sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end




"""
brute force search without larger skips
"""
function _searchindex_v0(s::ByteArray, t::ByteArray, i::Integer,sv::Vector)
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

    tlast = _nthbyte(t,n)
    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i <= w
        loops += 1
        if _nthbyte(s,i+n) == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end
            # match found
            if j == n - 1
                return i+1
            end
        end
        i += 1
    end
    sv[Int(SFloops)] = loops
    #sv[Int(SFbloomtests)] = bloomtests
    #sv[Int(SFbloomskips)] = bloomskips
    #sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end




"""
variant: try to optimize v2
"""
function _searchindex_v5(s::ByteArray, t::ByteArray, i::Integer,sv::Vector)
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

    skip = n # -1 removed because unconditional i +=1 at end of main loop removed
    tlast = _nthbyte(t,n)
    bloom_mask = UInt64(_search_bloom_mask(tlast))
    for j in 1:n-1 # n-1: last byte of pattern is already in bloom filter
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast
            skip = n - j # -1 removed because unconditional i +=1 at end of main loop removed
        end
    end
    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i <= w
        loops += 1
        silast =  _nthbyte(s,i+n)
        bloomtests += 1
        if bloom_mask & _search_bloom_mask(silast) == 0
            bloomskips += 1
            i += n
            continue
        end
        if silast == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
                # match found?
                if j == n - 1
                    return i+1
                end
            end
            # no match, use skip distance
            # original code does bloom test assuming skip==1 here
            # better postpone that to next ineration
            i += skip
            continue
        end
        i += 1
    end
    sv[Int(SFloops)] = loops
    sv[Int(SFbloomtests)] = bloomtests
    sv[Int(SFbloomskips)] = bloomskips
    sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end



"""
variant: try to optimize v2 (no silast temp variable)
"""
function _searchindex_v6(s::ByteArray, t::ByteArray, i::Integer,sv::Vector)
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

    skip = n # -1 removed because unconditional i +=1 at end of main loop removed
    tlast = _nthbyte(t,n)
    bloom_mask = UInt64(_search_bloom_mask(tlast))
    for j in 1:n-1 # n-1: last byte of pattern is already in bloom filter
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast
            skip = n - j # -1 removed because unconditional i +=1 at end of main loop removed
        end
    end
    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i < w
        loops += 1
        bloomtests += 1
        silast = _nthbyte(s,i+n)
        if bloom_mask & _search_bloom_mask(silast) == 0
            bloomskips += 1
            i += n
            continue
        end
        if silast == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
                # match found?
                if j == n - 1
                    return i+1
                end
            end
            # no match, use skip distance
            # original code does bloom test assuming skip==1 here
            # better postpone that to next ineration
            i += skip
            continue
        end
        i += 1
        # main loop unrolled once: because "i<w" we have guaranteed i<=w
        bloomtests += 1
        silast = _nthbyte(s,i+n)
        if bloom_mask & _search_bloom_mask(silast) == 0
            bloomskips += 1
            i += n
            continue
        end
        if silast == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
                # match found?
                if j == n - 1
                    return i+1
                end
            end
            # no match, use skip distance
            # original code does bloom test assuming skip==1 here
            # better postpone that to next ineration
            i += skip
            continue
        end
    end
    sv[Int(SFloops)] = loops
    sv[Int(SFbloomtests)] = bloomtests
    sv[Int(SFbloomskips)] = bloomskips
    sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end

function runbench(f::Function,s::ByteArray,t::ByteArray, stats::Stats)
    #print(string(f),": ")
    sf = zeros(Int,Int(typemax(StatsFields)))
    elapsedtime = time_ns()
    pos=f(s,t,1,sf)
    sf[Int(SFtime)] = (time_ns() - elapsedtime)%Int
    record(stats,f,sizeof(t),sf)
    pos
end


function benchmark(alphabetsize::Int , patternsize::Int, textsize::Int, statsfile::String)
    s = rand(0x00:alphabetsize%UInt8,textsize) # array to search in
    t = rand(0x00:alphabetsize%UInt8,patternsize) # pattern to search
    stats = Stats(patternsize)
    for j in 4:patternsize
        tj = t[1:j] # does allocation here has any effect on benchmarking??!
        #println()
        #println("benchmark with pattern size $j")
        p0 = runbench(_searchindex_julia,s,tj,stats)
        p1 = runbench(_searchindex_v1,s,tj,stats)
        p2 = runbench(_searchindex_v2,s,tj,stats)
        p3 = runbench(_searchindex_v3,s,tj,stats)
        p4 = runbench(_searchindex_v4,s,tj,stats)
        p5 = runbench(_searchindex_v5,s,tj,stats)
        p5 = runbench(_searchindex_v6,s,tj,stats)
        p5 = runbench(_searchindex_v0,s,tj,stats)
        if p0!=p1 || p1!=p2 || p2!=p3 || p3!=p4
            println("ERROR p0=$p0, p1=$p1, p2=$p2, p3=$p3, p4=$p4")
        else
            #println("found at $p0")
        end
    end
    if statsfile!=""
        io = open(statsfile, "w")
        print(io,stats)
        close(io)
    end
end

benchmark(128,3,100,"")