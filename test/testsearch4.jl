# include("C:\\Users\\RR\\.julia\\dev\\Tokens\\test\\testsearch4.jl")

#=
Ergebnis:

v1 hat das "RR-Verhalten": die ersten paar iter-s besser,
dann lange deutlich schlechter.

v2 ist wie julia

v3 ist signifikant um die 2% besser, wegen der skip-optimierung

Auffällig: die Stats zu loop,test,skip sind bei ju und v1,v2 IDENTISCH
Das macht das Laufzeitverhalten von v1 noch mysteriöser.

=#
# comparing _searchindex from base.search.jl with variants
# Author Robert Rudolph www.r2c.de
import Base.ByteArray, Base._nthbyte, Base._search_bloom_mask
using Plots

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

const nullmat = zeros(Int,2)

function record(stats::Stats, f::Function, i::Int, sv::Vector)
    mat = get(stats.dict,f,nullmat)
    if mat===nullmat
        mat = copy(get(stats.dict,zeros,nullmat))
        push!(stats.dict,f=>mat)
    end
    mat[i,:] = sv
end


"return stats series for function and field"
function line(stats::Stats,f::Function, field::StatsFields)
    m = get(stats.dict,f,nullmat)
    m[:,Int(field)]
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



# skip-optimized julia version
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

    skip = n - 1
    tlast = _nthbyte(t,n)
    bloom_mask = UInt64(_search_bloom_mask(tlast))
    for j in 1:n-1
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast
            skip = n - j - 1
        end
    end
    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i < w
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

            # no match: skip and test bloom
            i += skip
            bloomtests += 1
            if i<w && bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                bloomskips += 1
                i += n
            end
        else
            bloomtests += 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                bloomskips += 1
                i += n
            end
        end
        i += 1
    end
    if i==w
        # test end match
        j = 1
        while j < n
            if _nthbyte(s,i+j) != _nthbyte(t,j)
                break # not found
            end
            j += 1
        end
        if j == n
            return i+1
        end # match at the very end
    end
    sv[Int(SFloops)] = loops
    sv[Int(SFbloomtests)] = bloomtests
    sv[Int(SFbloomskips)] = bloomskips
    sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end



"loop-optimized version"
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

    tlast = _nthbyte(t,n)
    bloom_mask =  _search_bloom_mask(tlast)
    skip = n
    for j in 1:n-1
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast
            skip = n - j
        end
    end
    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i < w
        loops += 1
        if _nthbyte(s,i+n) == tlast
            # check candidate
            j = 1
            while j < n
                if _nthbyte(s,i+j) != _nthbyte(t,j)
                    break
                end
                j += 1
                # match found?
                if j == n
                    return i+1 # no stats update
                end
            end
            # no match, try to rule out the next character
            bloomtests += 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                bloomskips += 1
                i += n+1
            else
                i += skip
            end
        else
            i += 1 # due to byte mismatch
            bloomtests += 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n)) == 0
                bloomskips += 1
                i += n
            end
        end
    end
    if i==w
        # test end match
        j = 1
        while j < n
            if _nthbyte(s,i+j) != _nthbyte(t,j)
                break # not found
            end
            j += 1
        end
        if j == n
            return i+1
        end # match at the very end
    end

    sv[Int(SFloops)] = loops
    sv[Int(SFbloomtests)] = bloomtests
    sv[Int(SFbloomskips)] = bloomskips
    sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end


# skip-optimized, check für found
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
    skip = n - 1
    tlast = _nthbyte(t,n)
    bloom_mask = UInt64(_search_bloom_mask(tlast))
    for j in 1:n-1
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast
            skip = n - j - 1
        end
    end
    loops = 0
    bloomtests = 0
    bloomskips = 0

    i -= 1
    while i < w
        loops += 1
        if _nthbyte(s,i+n) == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
                # match found
                if j == n - 1
                    return i+1 # no stats update
                end
            end

            # no match: skip and test bloom
            i += skip
            bloomtests += 1
            if i<w && bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                bloomskips += 1
                i += n
            end
        else
            bloomtests += 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                bloomskips += 1
                i += n
            end
        end
        i += 1
    end
    if i==w
        # test end match
        j = 1
        while j < n
            if _nthbyte(s,i+j) != _nthbyte(t,j)
                break # not found
            end
            j += 1
        end
        if j == n
            return i+1
        end # match at the very end
    end
    sv[Int(SFloops)] = loops
    sv[Int(SFbloomtests)] = bloomtests
    sv[Int(SFbloomskips)] = bloomskips
    sv[Int(SFbloombits)] = bitcount(bloom_mask)
    0
end


# skip-optimized julia version,
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

    skip = n
    tlast = _nthbyte(t,n)
    bloom_mask = UInt64(_search_bloom_mask(tlast))
    for j in 1:n-1
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast
            skip = n - j
        end
    end
    loops = 0
    bloomtests = 0
    bloomskips = 0
    i -= 1
    while i < w
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

            # no match: skip and test bloom
            i += skip
            bloomtests += 1
            if i<w && bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                bloomskips += 1
                i += n
            end
        else
            i += 1
            bloomtests += 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n)) == 0
                bloomskips += 1
                i += n
            end
        end
    end
    if i==w
        # test end match
        j = 1
        while j < n
            if _nthbyte(s,i+j) != _nthbyte(t,j)
                break # not found
            end
            j += 1
        end
        if j == n
            return i+1
        end # match at the very end
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
#        p4 = runbench(_searchindex_v4,s,tj,stats)
#        p5 = runbench(_searchindex_v5,s,tj,stats)
#        p5 = runbench(_searchindex_v6,s,tj,stats)
#        p5 = runbench(_searchindex_v0,s,tj,stats)
        if p0!=p1 || p1!=p2 || p2!=p3
            println("ERROR p0=$p0, p1=$p1, p2=$p2, p3=$p3")
        else
            #println("found at $p0")
        end
    end
    if statsfile!=""
        io = open(statsfile, "w")
        print(io,stats)
        close(io)
    end

    # plot
    julia = line(stats,_searchindex_julia,SFtime)
    v1 = line(stats,_searchindex_v1,SFtime)

    pp =plot(julia, label=string(_searchindex_julia))
    plot!(v1, label=string(_searchindex_v1))
    plot!(line(stats,_searchindex_v2,SFtime), label=string(_searchindex_v2))
    plot!(line(stats,_searchindex_v3,SFtime), label=string(_searchindex_v3))
    display(pp)
end

#benchmark(128,3,100,"r:\\testfile.txt")
