# include("C:\\Users\\RR\\.julia\\dev\\Tokens\\test\\countsearch.jl")

# comparing _searchindex from base.search.jl with variants
# Author Robert Rudolph www.r2c.de
import Base.ByteArray, Base._nthbyte, Base._search_bloom_mask

mutable struct LocateStats
    compareLastByte :: Int
    compareOtherByte :: Int
    bloomTest :: Int
    bloomSkip :: Int
    bloomhalf :: Int
    skips :: Int
end
LocateStats()=LocateStats(0,0,0,0,0,0)



function _searchindex(s::String, t::String, i::Integer)
    # Check for fast case of a single byte
    lastindex(t) == 1 && return something(findnext(isequal(t[1]), s, i), 0)
    _searchindex(unsafe_wrap(Vector{UInt8},s), unsafe_wrap(Vector{UInt8},t), i)
end

function _searchindex_julia(s::ByteArray, t::ByteArray, i::Integer,ls::LocateStats)
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
        ls.compareLastByte += 1
        if _nthbyte(s,i+n) == tlast
            # check candidate
            j = 0
            ls.compareOtherByte += 1
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

            # no match, try to rule out the next character
            ls.bloomTest += 1
            if i < w && bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                i += n
                ls.bloomSkip += 1
            else
                i += skip
            end
        elseif i < w
            ls.bloomTest += 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                ls.bloomSkip += 1
                i += n
            end
        end
        i += 1
    end

    0
end

"""
variant: bloomfilter without last byte of pattern, apply
filter on tested byte (instead following byte)
"""
function _searchindex_v1(s::ByteArray, t::ByteArray, i::Integer,ls::LocateStats)
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

    i -= 1
    while i <= w
        silast =  _nthbyte(s,i+n)
        ls.compareLastByte += 1
        if silast == tlast
            # check candidate
            j = 0
            ls.compareOtherByte += 1
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
        else
            ls.bloomTest += 1
            if bloom_mask & _search_bloom_mask(silast) == 0
                ls.bloomSkip += 1
                i += n
            else # worst case: minimal skip
                i += 1
            end
        end
    end

    0
end






"""
variant: test bloomfilter first
"""
function _searchindex_v2(s::ByteArray, t::ByteArray, i::Integer,ls::LocateStats)
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

    i -= 1
    while i <= w
        silast =  _nthbyte(s,i+n)
        ls.bloomTest += 1
        if bloom_mask & _search_bloom_mask(silast) == 0
            ls.bloomSkip += 1
            i += n
        else
            ls.compareLastByte += 1
            if silast == tlast
                # check candidate
                j = 0
                ls.compareOtherByte += 1
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
    end

    0
end



"""
variant: reordered v2
"""
function _searchindex_v2a(s::ByteArray, t::ByteArray, i::Integer,ls::LocateStats)
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

    i -= 1
    while i <= w
        silast =  _nthbyte(s,i+n)
        ls.bloomTest += 1
        if bloom_mask & _search_bloom_mask(silast) == 0
            ls.bloomSkip += 1
            i += n
            continue
        end
        ls.compareLastByte += 1
        if silast != tlast
            # worst case: minimal skip
            i += 1
            continue
        end
        # check candidate
        j = 0
        ls.compareOtherByte += 1
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
    end
    0
end

const optimal_bloom_size = 32 # best guess: max of p(not in bloom)*size(bloom

"""
variant: like v2 but avoid bloom filter degeneration on large pattern sizes
"""
function _searchindex_v3(s::ByteArray, t::ByteArray, i::Integer,ls::LocateStats)
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
    for j in 1:n-1 # n-1: last byte of pattern is already in bloom filter
        if (j>n-optimal_bloom_size)
            bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
            bloom_size += 1
        end
        if _nthbyte(t,j) == tlast
            skip = n - j # -1 removed because unconditional i +=1 at end of main loop removed
        end
    end

    i -= 1
    while i <= w
        silast =  _nthbyte(s,i+n)
        ls.bloomTest += 1
        if bloom_mask & _search_bloom_mask(silast) == 0
            ls.bloomSkip += 1
            i += bloom_size
        else
            ls.compareLastByte += 1
            if silast == tlast
                # check candidate
                ls.compareOtherByte += 1
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
    end

    0
end


"""
variant: like v3 plus v2 (2 bloom filter)
"""
function _searchindex_v4(s::ByteArray, t::ByteArray, i::Integer,ls::LocateStats)
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
    bloom_32 = bloom_mask
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

    i -= 1
    while i <= w
        silast =  _nthbyte(s,i+n)
        ls.bloomTest += 1
        hash =  _search_bloom_mask(silast)
        if bloom_mask & hash == 0
            ls.bloomSkip += 1
            i += n
        else
            ls.bloomTest += 1
            if (bloom_half & hash) == 0
                i += bloom_size
                ls.bloomhalf += 1
            else
                ls.compareLastByte += 1
                if silast == tlast
                    # check candidate
                    ls.compareOtherByte += 1
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
        end
    end

    0
end


function runbench(f::Function,s::ByteArray,t::ByteArray)
    print(string(f),": ")
    l=LocateStats()
    elapsedtime = time_ns()
    pos=f(s,t,1,l)
    print(time_ns() - elapsedtime)
    println(" last==",l.compareLastByte,", other==",l.compareOtherByte,", bloom==", l.bloomTest, " #",l.bloomSkip, " ^",l.bloomhalf)
    pos
end


function benchmark(alphabetsize::Int,patternsize::Int, textsize::Int)
    s = rand(0x00:alphabetsize%UInt8,textsize) # array to search in
    t = rand(0x00:alphabetsize%UInt8,patternsize) # pattern to search
    for j in 1:patternsize # pattern size
        if j==1
            tj = unsafe_wrap(Vector{UInt8},"world")
            sj = unsafe_wrap(Vector{UInt8},"hello, world")
        else
            tj = t[1:j]
            sj = s
        end
        println()
        println("benchmark with pattern size $j")
        p0 = runbench(_searchindex_julia,sj,tj)
        p1 = runbench(_searchindex_v1,sj,tj)
        p2 = runbench(_searchindex_v2,sj,tj)
        p2 = runbench(_searchindex_v2a,sj,tj)
        p3 = runbench(_searchindex_v3,sj,tj)
        p4 = runbench(_searchindex_v4,sj,tj)
        if p0!=p1 || p1!=p2 || p2!=p3
            println("ERROR p0=$p0, p1=$p1, p2=$p2, p3=$p3")
        else
            println("found at $p0")
        end
    end
end

benchmark(128,3,100)
